import os
import shutil
import time
import subprocess
import soundfile as sf
import pynvml
import json
from mutagen.mp3 import MP3
from inputimeout import inputimeout, TimeoutOccurred
from tkinter import Tk
from tkinter.filedialog import askdirectory
from datetime import datetime, timedelta
from faster_whisper import WhisperModel


def get_gpu_memory():
    # Initialize the NVML library
    pynvml.nvmlInit()

    # Get the number of GPUs in the system
    device_count = pynvml.nvmlDeviceGetCount()

    # Store the GPU memory information
    gpu_memory = {}

    for i in range(device_count):
        handle = pynvml.nvmlDeviceGetHandleByIndex(i)
        memory_info = pynvml.nvmlDeviceGetMemoryInfo(handle)

        # Convert memory from bytes to GB
        total_memory_gb = memory_info.total / (1024 ** 3)
        gpu_memory[i] = total_memory_gb

    # Shutdown the NVML library
    pynvml.nvmlShutdown()

    return gpu_memory


def delete_empty_folders(directory, exceptions=None):
    """Delete all empty folders in the given directory, excluding exceptions."""
    exceptions = exceptions or []
    for root, dirs, files in os.walk(directory, topdown=False):
        for d in dirs:
            folder_path = os.path.join(root, d)
            if folder_path not in exceptions and not os.listdir(folder_path):
                os.rmdir(folder_path)
                print(f"[INFO] Deleted empty folder: {folder_path}")


def delete_transcribing_files(queue_dir):
    """Delete all files in queue_dir that start with 'transcribing'."""
    for file in os.listdir(queue_dir):
        if file.startswith("transcribing"):
            os.remove(os.path.join(queue_dir, file))
            print(f"[INFO] Deleted file: {file}")


def process_audio_file(file, queue_dir, transcribed_dir, sample_rate):
    """Process an audio file by compressing/copying and transcribing it."""
    file_path = os.path.join(queue_dir, file)
    file_name, ext = os.path.splitext(file)
    dest_dir = os.path.join(transcribed_dir, file_name)
    dest_file = os.path.join(dest_dir, f"transcribing.mp3")
    
    try:
        ## Ensure destination directory is either empty or only contains audio file named transcribing
        if os.path.exists(dest_dir):
            dest_ls = [file for file in os.listdir(dest_dir) if file != "desktop.ini"]
            if len(dest_ls) > 1 and not dest_ls == [os.path.basename(dest_file)]:
                raise Exception(f"[ERROR] Folder '{dest_dir}' exists and has contents")
        else:
            os.makedirs(dest_dir)

        ## Compress and/or copy
        if os.path.basename(dest_file) not in os.listdir(dest_dir):
            ## soundfile can only check mp3 files for current sample rate
            if os.path.splitext(file)[1] == "mp3":
                _, sr0 = sf.read(file_path)
            else:
                sr0 = 48 * 1e3  # Always compress non-mp3 files
            if sr0 > sample_rate:
                cmd = f'ffmpeg -nostdin -threads 0 -i "{file_path}" -ac 1 -ar {sample_rate} "{dest_file}" -hide_banner -loglevel error'
                subprocess.run(cmd, shell=True, check=True)
                print(f"[INFO] Compressed {file} -> {dest_file}")
            else:
                shutil.copy(file_path, dest_file)  # Copy mp3 files with acceptable sample rate
                print(f"[INFO] Copied {file} -> {dest_file}")
        
        ## Transcription command
        print(f"[INFO] Transcribing {dest_file}")
        model_size = "large-v3-turbo"
        compute_type="float16"
        model = WhisperModel(model_size, device="cuda", compute_type=compute_type)
        segments, info = model.transcribe(dest_file)

        print("[INFO] Detected language '%s' with probability %f" % (info.language, info.language_probability))
        lang_prob_threshold = 0.9
        if info.language_probability < lang_prob_threshold:
            try:
                bool_cont = inputimeout(prompt=f"Language probability is below {lang_prob_threshold}. Do you wish to continue? (y/n) ", timeout=10) or "y"
                if bool_cont == "n":
                    exit()
            except TimeoutOccurred:
                print("Timeout occurred. No input received.")

        def iterate_segment(segment):
            print("[%.2fs -> %.2fs] %s" % (segment.start, segment.end, segment.text))
            return segment.__dict__

        audio_dur = MP3(dest_file).info.length
        segments = [iterate_segment(segment) for segment in segments if segment.start < audio_dur]
        text = "".join([segment["text"] for segment in segments]).strip()
        result = {
            "text": text,
            "segments": segments,
            "language": info.language,
            "language_probability": info.language_probability
        }
        json_result = open(os.path.join(dest_dir, "result.json"), "w")
        json.dump(result, json_result, default=lambda o: "<not serializable>")
        json_result.close()

        ## Write info
        json_info = open(os.path.join(dest_dir, "info.json"), "w")
        json.dump(info.__dict__, json_info, default=lambda o: "<not serializable>")
        json_info.close()

        print(f"[INFO] Transcription completed for: {dest_file}")

        rename_and_move_transcriptions(dest_dir, transcribed_dir, file_name, ext)
        
        os.remove(file_path)
        print(f"[INFO] Deleted original audio file: {file}")
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] Error processing {file}: {e}")


def rename_and_move_transcriptions(queue_dir, transcribed_dir, file_name, ext):
    """Rename transcribed files and move them to a folder."""
    target_folder = os.path.join(transcribed_dir, f"{file_name}")
    os.makedirs(target_folder, exist_ok=True)
    
    for file in os.listdir(queue_dir):
        if file.startswith("transcribing"):
            new_name = f"{file_name}{os.path.splitext(file)[1]}"
            shutil.move(os.path.join(queue_dir, file), os.path.join(target_folder, new_name))
            print(f"[INFO] Moved {file} -> {target_folder}/{new_name}")


def monitor_queue(queue_dir, transcribed_dir, interval, sample_rate):
    """Continuously check for audio files and process them."""
    os.chdir(queue_dir)
    queue_dir = os.getcwd();  # Refresh for if relative path provided
    print(f"[INFO] Working directory set to: {queue_dir}")
    
    while True:
        audio_files = [f for f in os.listdir(queue_dir) if f.endswith((".mp3", ".m4a", ".wav", ".flac"))]
        if not audio_files:
            next_check = datetime.now() + timedelta(seconds=interval)
            print(f"[INFO] No audio files found. Checking again in {interval} seconds at {next_check.strftime('%Y-%m-%d %H:%M:%S')}")
            time.sleep(interval)
            continue
        
        delete_empty_folders(transcribed_dir, exceptions=[os.path.join(queue_dir, "transcribed")])
        delete_transcribing_files(queue_dir)
        
        print(f"[INFO] Transcribing {len(audio_files)} audio files")
        
        for file in audio_files:
            process_audio_file(file, queue_dir, transcribed_dir, sample_rate)


def get_dir(default_dir: str="browse", label: str="", timeout: int=10, bool_msg: bool=False):
    """Get input directory."""
    if default_dir != "browse":
        default_dir = os.path.abspath(default_dir)
    if bool_msg:
        default_msg = f" (leave blank for default: {default_dir})"
    else:
        default_msg = ""  # Deactivate showing default
    if len(label) > 0:
        label = label + " "
    try:
        dir = inputimeout(prompt=f"Enter {label}directory{default_msg}: ", timeout=timeout).strip() or default_dir
    except TimeoutOccurred:
        dir = default_dir
        print(f"[WARNING] Timeout occurred; defaulting to {default_dir}")
    if dir == "browse":
        dir = askdirectory(title=f"Select Folder")
    return dir


def main():
    """Continuously check for audio files and process them."""
    ## Acquire inputs
    queue_dir = get_dir(os.path.abspath(os.path.join('..', '..', 'sermons')), "queue")
    transcribed_dir = get_dir(os.path.join(queue_dir, "transcribed"), "transcribed")
    
    interval = int(input("Enter interval (in seconds) for checking new files: ").strip() or 10)
    sample_rate = 16000  # Default sample rate
    
    os.makedirs(transcribed_dir, exist_ok=True)
    print(f"[INFO] Monitoring queue at: {queue_dir}, Transcribed files will be saved to: {transcribed_dir}")
    
    monitor_queue(queue_dir, transcribed_dir, interval, sample_rate)


if __name__ == "__main__":
    main()

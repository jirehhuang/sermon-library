import os
import shutil
import time
import subprocess
import soundfile as sf
import pynvml
from tkinter import Tk
from tkinter.filedialog import askdirectory
from datetime import datetime, timedelta


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


def process_audio_file(file, queue_dir, transcribed_dir, bool_compress, sample_rate):
    """Process an audio file by compressing/copying and transcribing it."""
    file_path = os.path.join(queue_dir, file)
    file_name, ext = os.path.splitext(file)
    dest_dir = os.path.join(transcribed_dir, file_name)
    dest_file = os.path.join(dest_dir, f"transcribing{ext}")
    
    try:
        ## Ensure destination directory is either empty or only contains audio file named transcribing
        if os.path.exists(dest_dir):
            if len(os.listdir(dest_dir)) > 1 or not os.listdir(dest_dir) == [os.path.basename(dest_file)]:
                raise Exception(f"Error: Folder '{dest_dir}' exists and has contents")
        else:
            os.makedirs(dest_dir)

        ## Compress and/or copy
        if os.path.basename(dest_file) not in os.listdir(dest_dir):
            if bool_compress:
                if os.path.splitext(file)[1] == "mp3":
                    _, sr0 = sf.read(file_path)
                else:
                    sr0 = 48 * 1e3
                if sr0 > sample_rate:
                    cmd = f'ffmpeg -nostdin -threads 0 -i "{file_path}" -ac 1 -ar {sample_rate} "{dest_file}" -hide_banner -loglevel error'
                    subprocess.run(cmd, shell=True, check=True)
                    print(f"[INFO] Compressed {file} -> {dest_file}")
                else:
                    shutil.copy(file_path, dest_file)
                    print(f"[INFO] Copied {file} -> {dest_file}")
            else:
                shutil.copy(file_path, dest_file)
                print(f"[INFO] Copied {file} -> {dest_file}")
        
        ## Transcription command
        print(f"[INFO] Transcribing {dest_file}")
        transcribe_cmd = f'whisper "{dest_file}" --output_dir "{dest_dir}" --device cuda --model medium.en --language en --verbose True'
        subprocess.run(transcribe_cmd, shell=True, check=True)
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


def monitor_queue(queue_dir, transcribed_dir, interval, bool_compress, sample_rate):
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
        
        for file in audio_files:
            process_audio_file(file, queue_dir, transcribed_dir, bool_compress, sample_rate)


def get_dir(default_dir: str="browse", label: str="", bool_msg: bool=False):
    """Get input directory."""
    if default_dir != "browse":
        default_dir = os.path.abspath(default_dir)
    if bool_msg:
        default_msg = f" (leave blank for default: {default_dir})"
    else:
        default_msg = ""  # Deactivate showing default
    if len(label) > 0:
        label = label + " "
    dir = input(f"Enter {label}directory{default_msg}: ").strip() or default_dir
    if dir == "browse":
        dir = askdirectory(title=f"Select Folder")
    return dir


def main():
    """Continuously check for audio files and process them."""
    ## Acquire inputs
    queue_dir = get_dir(os.path.abspath(os.path.join('..', '..', 'sermons')), "queue")
    transcribed_dir = get_dir(os.path.join(queue_dir, "transcribed"), "transcribed")
    
    interval = int(input("Enter interval (in seconds) for checking new files: ").strip() or 10)
    bool_compress = input("Compress audio files? (y/n): ").strip().lower() == "y" or True
    sample_rate = 16000  # Default sample rate
    
    os.makedirs(transcribed_dir, exist_ok=True)
    print(f"[INFO] Monitoring queue at: {queue_dir}, Transcribed files will be saved to: {transcribed_dir}")
    
    monitor_queue(queue_dir, transcribed_dir, interval, bool_compress, sample_rate)


if __name__ == "__main__":
    main()

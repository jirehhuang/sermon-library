import os
import shutil
import time
import subprocess
from tkinter import Tk
from tkinter.filedialog import askdirectory
from datetime import datetime, timedelta


def delete_empty_folders(directory, exceptions=None):
    """Delete all empty folders in the given directory, excluding exceptions."""
    exceptions = exceptions or []
    for root, dirs, files in os.walk(directory, topdown=False):
        for d in dirs:
            folder_path = os.path.join(root, d)
            if folder_path not in exceptions and not os.listdir(folder_path):
                os.rmdir(folder_path)
                print(f"[INFO] Deleted empty folder: {folder_path}")


def delete_whispering_files(queue_dir):
    """Delete all files in queue_dir that start with 'whispering'."""
    for file in os.listdir(queue_dir):
        if file.startswith("whispering"):
            os.remove(os.path.join(queue_dir, file))
            print(f"[INFO] Deleted file: {file}")


def process_audio_file(file, queue_dir, transcribed_dir, bool_compress, sample_rate):
    """Process an audio file by compressing/copying and transcribing it."""
    file_path = os.path.join(queue_dir, file)
    file_name, ext = os.path.splitext(file)
    dest_file = os.path.join(queue_dir, f"whispering{ext}")
    
    try:
        if bool_compress:
            cmd = f'ffmpeg -nostdin -threads 0 -i "{file_path}" -ac 1 -ar {sample_rate} "{dest_file}"'
            subprocess.run(cmd, shell=True, check=True)
            print(f"[INFO] Compressed {file} -> {dest_file}")
        else:
            shutil.copy(file_path, dest_file)
            print(f"[INFO] Copied {file} -> {dest_file}")
        
        ## Transcription command
        transcribe_cmd = f'whisper "{dest_file}" --model base --language en --verbose True'
        subprocess.run(transcribe_cmd, shell=True, check=True)
        print(f"[INFO] Transcription completed for: {dest_file}")
        
        rename_and_move_transcriptions(queue_dir, transcribed_dir, file_name, ext)
        
        os.remove(file_path)
        print(f"[INFO] Deleted original audio file: {file}")
    except subprocess.CalledProcessError as e:
        print(f"[ERROR] Error processing {file}: {e}")


def rename_and_move_transcriptions(queue_dir, transcribed_dir, file_name, ext):
    """Rename transcribed files and move them to a timestamped folder."""
    timestamp = datetime.now().strftime("%Y-%m-%d %H-%M-%S")
    target_folder = os.path.join(transcribed_dir, f"{file_name} - {timestamp}")
    os.makedirs(target_folder, exist_ok=True)
    
    for file in os.listdir(queue_dir):
        if file.startswith("whispering"):
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
        delete_whispering_files(queue_dir)
        
        for file in audio_files:
            process_audio_file(file, queue_dir, transcribed_dir, bool_compress, sample_rate)


def main():
    ## Acquire inputs
    queue_dir = input("Enter queue directory to monitor: ").strip() or "../../sermons"
    if (queue_dir == "browse"):
        queue_dir = askdirectory(title="Select Folder")
    
    default_transcribed_dir = f" (leave blank for default: {os.path.join(queue_dir, 'transcribed')})"
    default_transcribed_dir = ""  # Deactivate showing default
    transcribed_dir = input(f"Enter transcribed directory{default_transcribed_dir}: ").strip() or os.path.join(queue_dir, "transcribed")
    if (transcribed_dir == "browse"):
        transcribed_dir = askdirectory(title="Select Folder")
    
    interval = int(input("Enter interval (in seconds) for checking new files: ").strip() or 10)
    bool_compress = input("Compress audio files? (y/n): ").strip().lower() == "y" or True
    sample_rate = 16000  # Default sample rate
    
    os.makedirs(transcribed_dir, exist_ok=True)
    print(f"[INFO] Monitoring queue at: {queue_dir}, Transcribed files will be saved to: {transcribed_dir}")
    
    monitor_queue(queue_dir, transcribed_dir, interval, bool_compress, sample_rate)


if __name__ == "__main__":
    main()

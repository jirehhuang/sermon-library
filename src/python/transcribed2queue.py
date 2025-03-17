import os
import shutil
from transcriber import get_dir


def transcribed2queue(transcribed_dir, queue_dir):
    """
    Copies the audio file from each folder in transcribed_dir to queue_dir.
    
    Args:
        transcribed_dir (str): Path to the directory containing transcribed folders.
        queue_dir (str): Path to the queue directory where audio files will be copied.
    """
    # Ensure queue_dir exists
    os.makedirs(queue_dir, exist_ok=True)
    
    # Supported audio file extensions
    audio_extensions = ('.mp3', '.m4a', '.wav', '.flac')

    # Loop through each folder in transcribed_dir
    for folder_name in os.listdir(transcribed_dir):
        folder_path = os.path.join(transcribed_dir, folder_name)

        # Ensure it's a directory
        if not os.path.isdir(folder_path):
            continue

        # Find the first audio file in the folder
        audio_file = None
        for file in os.listdir(folder_path):
            if file.lower().endswith(audio_extensions):
                audio_file = file
                break

        # If no audio file is found, skip this folder
        if audio_file is None:
            print(f"Skipping {folder_name}: No audio file found.")
            continue

        # Copy the audio file to queue_dir
        src_path = os.path.join(folder_path, audio_file)
        dest_path = os.path.join(queue_dir, audio_file)

        try:
            shutil.copy2(src_path, dest_path)
            print(f"Copied {audio_file} from {folder_name} to queue.")
        except Exception as e:
            print(f"Error copying {audio_file} from {folder_name}: {e}")


def main():
    queue_dir = get_dir(os.path.abspath(os.path.join('..', '..', 'sermons')), "queue")
    transcribed_dir = get_dir(os.path.join(queue_dir, "transcribed"), "transcribed")
    transcribed2queue(transcribed_dir, queue_dir)


if __name__ == "__main__":
    main()

import os
import json
from mutagen.mp3 import MP3
from transcriber import get_dir


def trim_segments(directory):
    """
    Loops through all folders in the given directory, processes the .mp3 file to get its duration,
    filters the segments in result.json based on duration, and updates the file in place.

    Args:
        directory (str): The path to the directory containing subfolders.
    """
    for folder_name in os.listdir(directory):
        folder_path = os.path.join(directory, folder_name)

        ## Ensure it is a directory
        if not os.path.isdir(folder_path):
            continue

        ## Identify the .mp3 file
        mp3_file = None
        for file in os.listdir(folder_path):
            if file.lower().endswith('.mp3'):
                mp3_file = os.path.join(folder_path, file)
                break

        if mp3_file is None:
            print(f"[INFO] Skipping {folder_name}: No .mp3 file found.")
            continue

        ## Get audio duration
        try:
            audio_dur = MP3(mp3_file).info.length
        except Exception as e:
            print(f"[INFO] Error reading {mp3_file}: {e}")
            continue

        ## Read result.json
        json_file = os.path.join(folder_path, "result.json")
        if not os.path.exists(json_file):
            print(f"[INFO] Skipping {folder_name}: result.json not found.")
            continue

        try:
            with open(json_file, "r", encoding="utf-8") as f:
                result = json.load(f)
        except Exception as e:
            print(f"[INFO] Error reading {json_file}: {e}")
            continue

        ## Ensure result["segments"] exists
        if "segments" not in result or not isinstance(result["segments"], list):
            print(f"[INFO] Skipping {folder_name}: 'segments' key missing or invalid in result.json.")
            ConnectionRefusedError

        print(f"[INFO] Trimming {folder_name}.")

        ## Subset segments where start < audio_dur
        len0 = len(result["segments"])
        result["segments"] = [segment for segment in result["segments"] if segment["start"] < audio_dur]
        len1 = len(result["segments"])

        if (len1 < len0):
            ## Update text field
            print(f"[INFO] Trimmed {len0 - len1} segments that start beyond {audio_dur}s")
            result["text"] = "".join([segment["text"] for segment in result["segments"]]).strip()

            ## Write updated result.json
            try:
                with open(json_file, "w", encoding="utf-8") as f:
                    json.dump(result, f, ensure_ascii=False, indent=2)
                print(f"[INFO] Updated result.json in {folder_name}.")
            except Exception as e:
                print(f"[ERROR] Error writing {json_file}: {e}")


def main():
    transcribed_dir = get_dir("browse", "transcribed")
    trim_segments(transcribed_dir)


if __name__ == "__main__":
    main()

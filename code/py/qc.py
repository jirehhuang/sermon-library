import os
import uuid
import json
import pandas as pd
import soundfile as sf
import sounddevice as sd
import tkinter as tk
import subprocess
from difflib import SequenceMatcher
from pydub import AudioSegment
from transcriber import get_dir


def json2segments(folder_name, json_file):
    """Extract transcription segments from JSON file."""
    with open(json_file, 'r', encoding='utf-8') as f:
        json_data = json.load(f)
    segments = pd.DataFrame(json_data["segments"])
    segments["name"] = folder_name
    segments["uid"] = segments["id"].apply(lambda x: str(uuid.uuid5(uuid.NAMESPACE_DNS, folder_name + " " + str(x))))
    segments["label"] = ""
    return segments

  
def sort_and_save_qc(qc, qc_file):
    """Sort and save the QC DataFrame."""
    qc.sort_values(by=["id", "name"], ascending=[True, True], inplace=True)
    qc.to_csv(qc_file, index=False)
    print(f"Saved QC file: {qc_file}")


def trim_audio(input_file, output_file, start_time, stop_time):
    """
    Trims an audio file between the specified start and stop times.
    
    :param input_file: Path to the input audio file
    :param output_file: Path to save the trimmed audio file
    :param start_time: Start time in seconds
    :param stop_time: Stop time in seconds
    """
    try:
        audio = AudioSegment.from_file(input_file)
        start_ms = start_time * 1000
        stop_ms = stop_time * 1000
        trimmed_audio = audio[start_ms:stop_ms]
        trimmed_audio.export(output_file, format="mp3")
        print(f"[INFO] Trimmed audio saved to {output_file}")
    except Exception as e:
        print(f"[ERROR] Failed to trim audio: {e}")


def play_audio(file_path, speed: float=1):
    """Play an audio file."""
    try:
        data, samplerate = sf.read(file_path)
        sd.play(data, samplerate * speed)
        return True
    except Exception as e:
        return False


def launch_audio(file_path):
    """
    Plays an audio file using the system's default media player.
    
    :param file_path: Path to the audio file
    """
    try:
        if os.name == "nt":
            os.startfile(file_path)
        elif os.name == "posix":
            subprocess.run(["open" if "darwin" in os.sys.platform else "xdg-open", file_path], check=True)
        print(f"[INFO] Playing {file_path}")
    except Exception as e:
        print(f"[ERROR] Failed to play audio: {e}")


def get_paragraph_input(window_title: str, initial_value: str) -> str:
    ## Create the root window with the given title
    root = tk.Tk()
    root.title(window_title)

    ## Create a Text widget for multi-line input
    text_box = tk.Text(root, height=10, width=40)
    text_box.insert(tk.END, initial_value)  # Prefill with the initial value
    text_box.pack(padx=10, pady=10)

    ## Set focus to the text box so the cursor is immediately in it
    text_box.focus()
    text_box.focus_force()

    ## Variable to store the user input
    user_input = None

    ## Function to get the input value and close the window
    def submit(event=None):  # Event parameter allows it to be used with keyboard bindings
        nonlocal user_input
        user_input = text_box.get("1.0", tk.END).strip()  # Get the input from the Text widget
        root.quit()

    ## Bind the Enter key to trigger the submit function
    root.bind('<Return>', submit)

    ## Add a submit button (optional, in case you want it as well)
    submit_button = tk.Button(root, text="Submit", command=submit)
    submit_button.pack(pady=5)

    ## Start the Tkinter event loop
    root.mainloop()
    try:
        root.destroy()
    except:
        pass

    ## Return the user input after the window is closed
    return user_input

        
def print_row_label(row):
    if not row.empty:
        if row["label"] == row["text"]:
            text = "[Accepted] " + row["label"]
        if row["label"] not in [""]:
            text = "[Labeled]  " + row["label"]
        else:
            text = "[Original] " + row["text"]
        print(f"{row['id']}. ({round(row['avg_logprob'], 3)}) [{row['start']}s to {row['end']}s]: {text}")


def qc_transcribed(transcribed_dir, qc_dir):
    """Main function for quality control of transcribed segments."""
    
    ## Ensure the given directory exists; create if not
    if not os.path.exists(transcribed_dir):
        os.makedirs(transcribed_dir)
        print(f"Created directory: {transcribed_dir}")
      
    ## Load existing QC file if it exists
    qc_file = os.path.join(qc_dir, "qc.csv")
    if os.path.exists(qc_file):
        qc0 = pd.read_csv(qc_file, na_filter=False)
    else:
        qc0 = pd.DataFrame()

    ## Compile and append new segments to qc
    qc1_list = []

    for folder_name in os.listdir(transcribed_dir):
        if not qc0.empty and folder_name in qc0["name"].values:
            continue
        folder_path = os.path.join(transcribed_dir, folder_name)
        if not os.path.isdir(folder_path) or folder_name == "qc":
            continue
        json_file = os.path.join(folder_path, f"result.json")
        if os.path.exists(json_file):
            qc1_list.append(json2segments(folder_name, json_file))
            print(f"Added segments from {json_file}")
    
    qc1 = pd.concat(qc1_list, ignore_index=True) if qc1_list else pd.DataFrame()
    if not qc1.empty:
        qc = pd.concat([qc0, qc1]).drop_duplicates(subset=["name", "id"], keep="first")
        sort_and_save_qc(qc, qc_file)
    else:
        print("No new segments found.")
        qc = qc0

    ## Strip whitespace
    qc["text"] = qc["text"].str.strip()
    qc["label"] = qc["label"].str.strip()

    ## Loop through segments by increasing avg_logprob
    qc.sort_values(by="avg_logprob", ascending=True, inplace=True)
    qc.reset_index(drop=True, inplace=True)
    labeled_count = qc["label"].str.strip().ne("").sum()
    print(f"{labeled_count} labeled segments out of {len(qc)}.")
    
    ## Determine whether or not to revisit previously labeled audio
    if labeled_count:
        bool_revisit = (input("Revisit previous labels? (y/n, default n): ") or "n").lower() == "y"
    else:
        bool_revisit = "n"  # Doesn't matter y or n

    ## Initialize
    temp_clip = os.path.join(qc_dir, "temp_clip.mp3")  # Initialize
    temp_context = os.path.join(qc_dir, "temp_context.mp3")  # Initialize

    for index, row in qc.iterrows():
        ## If not revisiting, skip already labeled rows
        if not bool_revisit and len(row["label"].strip()) > 0:
            continue

        ## Detect audio file, trim, and play
        audio_file = next((os.path.join(transcribed_dir, row["name"], f"{row['name']}.{ext}")
                           for ext in ["mp3", "m4a", "wav", "flac"]
                           if os.path.exists(os.path.join(transcribed_dir, row["name"], f"{row['name']}.{ext}"))), None)
        if not audio_file:
            print(f"Audio file missing for {row['name']}")
            continue
        
        trim_audio(audio_file, temp_clip, row["start"], row["end"])
        
        print(f"Segment {index+1} of {len(qc)}: ({round(row['avg_logprob'], 3)}) [{row['start']}s -> {row['end']}s] in {row['name']}")
        print(f"Transcription: {row['text']}")
        if len(row["label"]):
            sim = "%0.2f" % SequenceMatcher(None, row["text"], row["label"]).ratio()
            print(f"Label ({sim}):  {row['label']}")
        
        bool_played = play_audio(temp_clip, 1.5)  # Play sped up

        actions = "accept/replay/context/label/inaudible/save/skip/quit"
        while True:
            if (bool_played):
                action = input(f"Action ({actions}): ") or "accept"
            else:
                action = "inaudible"
            
            if action.lower() == "" or "accept".startswith(action.lower()):  # Default accept
                if len(row["label"]) and row["label"] != row["text"]:
                    print(f"Confirmed labeled transcription {row['uid']}: {row['label']}")
                else:
                    print(f"Accepted transcription {row['uid']}")
                    if not len(row["label"]):
                        qc.at[index, "label"] = row['text']
                uid_audio = os.path.join(qc_dir, f"{row['uid']}.{temp_clip.split('.')[-1]}")
                if not os.path.exists(uid_audio):
                    os.rename(temp_clip, uid_audio)
                break
            elif "replay".startswith(action.lower()):
                play_audio(temp_clip, 1)
            elif "context".startswith(action.lower()):
                ## Print transcriptions
                prev_row = qc[(qc["name"] == row["name"]) & (qc["id"] == row["id"]-1)]
                if not prev_row.empty:
                    prev_row = prev_row.iloc[0]
                next_row = qc[(qc["name"] == row["name"]) & (qc["id"] == row["id"]+1)]
                if not next_row.empty:
                    next_row = next_row.iloc[0]
                print_row_label(prev_row)
                print_row_label(row)
                print_row_label(next_row)
                ## Play audio
                if prev_row.empty:
                    prev_row = row
                if next_row.empty:
                    next_row = row
                trim_audio(audio_file, temp_context, 
                           min(row["start"], prev_row["start"]),
                           max(row["end"], next_row["end"]))
                launch_audio(temp_context)
            elif "inaudible".startswith(action.lower()):
                qc.at[index, "label"] = "<inaudible>"
                uid_audio = os.path.join(qc_dir, f"{row['uid']}.{temp_clip.split('.')[-1]}")
                if not os.path.exists(uid_audio):
                    os.rename(temp_clip, uid_audio)
                print(f"Labeled transcription {row['uid']} as inaudible")
                break
            elif "label".startswith(action.lower()):
                new_label = get_paragraph_input("Label Audio Transcription", row["text"])
                new_label = new_label.replace("\t", " ")  # Remove tab spaces
                new_label = new_label.replace("\n", " ")  # Remove line breaks
                while "  " in new_label:
                    new_label = new_label.replace("  ", " ")  # Remove double spaces
                qc.at[index, "label"] = new_label
                uid_audio = os.path.join(qc_dir, f"{row['uid']}.{temp_clip.split('.')[-1]}")
                if not os.path.exists(uid_audio):
                    os.rename(temp_clip, uid_audio)
                if new_label != row['text']:
                    if row["label"] == new_label:
                        print(f"Confirmed labeled transcription {row['uid']}: {new_label}")
                    else:
                        print(f"Labeled transcription {row['uid']}: {new_label}")
                else:
                    print(f"Accepted transcription {row['uid']}")
                break
            elif "save".startswith(action.lower()):
                sort_and_save_qc(qc, qc_file)
            elif "skip".startswith(action.lower()):
                qc.at[index, "label"] = "[skipped]"
                uid_audio = os.path.join(qc_dir, f"{row['uid']}.{temp_clip.split('.')[-1]}")
                if not os.path.exists(uid_audio):
                    os.rename(temp_clip, uid_audio)
                print(f"Skipped transcription {row['uid']}")
                break
            elif "quit".startswith(action.lower()) or "exit".startswith(action.lower()):
                break
            else:
                print(f"Input not recognized ({actions}): {action.lower()}")

        if "quit".startswith(action.lower()) or "exit".startswith(action.lower()):
            break
    
    sort_and_save_qc(qc, qc_file)
    os.remove(temp_clip)


def main():
    transcribed_dir = get_dir("browse", "transcribed")
    qc_dir = get_dir("browse", "qc")
    qc_transcribed(transcribed_dir, qc_dir)


if __name__ == "__main__":
    main()

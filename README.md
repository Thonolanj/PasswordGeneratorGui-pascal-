# Lazarus Password Generator

A GUI-based password generator written in Object Pascal using Lazarus/Free Pascal. This application allows users to generate one or more strong and customizable passwords, with options for character sets, length, and clipboard integration. User preferences are stored automatically for convenience.

## Features

- **Customizable Password Generation**
  - Select inclusion of uppercase, lowercase, numbers, full symbols, or custom symbol sets.
  - Specify password length and the number of passwords to generate.
  - Option to require the first character to be lowercase.
- **Clipboard Integration**
  - Copy all or selected passwords to the clipboard with a single click.
- **User Preferences**
  - Settings are saved to and loaded from an INI file (`PasswordGen.ini`) in the user's home directory.
- **Easy-to-use GUI**
  - Intuitive controls and real-time generation.

## How It Works

- Choose your desired password options using checkboxes and input fields.
- Click "Generate" to produce your passwords.
- Passwords are displayed in a list and automatically copied to your clipboard.
- Click the "Copy to Clipboard" button to copy selected or all passwords.
- User settings are saved automatically on close and reloaded on startup.

## Main Components

- **TForm1**: The main form, containing all controls and logic.
- **Character Sets**: Uppercase, lowercase, numbers, full symbols, or user-defined custom symbols.
- **Settings Storage**: Uses `TINIFile` to persist user preferences.
- **Password Logic**: Ensures selected character types are present and shuffles for randomness.

## Controls Overview

| Control           | Description                                 |
|-------------------|---------------------------------------------|
| UpCheck           | Include uppercase letters                   |
| loCheck           | Include lowercase letters                   |
| NumCheck          | Include numbers                             |
| FsCheck           | Include full symbol set                     |
| CSCheck           | Use custom symbols (from CustomSymbols edit)|
| loReq             | Require first character to be lowercase     |
| PasswordLength    | Set length of each password                 |
| NumberToGen       | Number of passwords to generate             |
| Gen               | Generate passwords                          |
| CopyToClipboard   | Copy to clipboard                           |
| Passwords         | List of generated passwords                 |

## File List

- `Unit1.pas` – Main form source code (application logic)
- `Unit1.lfm` – Form definition (GUI layout)
- `PasswordGen.ini` – User settings (auto-generated)

## Getting Started

### Requirements

- [Lazarus IDE](https://www.lazarus-ide.org/) (tested with Free Pascal Compiler)
- Windows, Linux, or macOS

### Build Instructions

1. Open `Unit1.pas` in Lazarus IDE.
2. Build the project (`Run` > `Build`).
3. Run the compiled executable.

### Usage

1. Launch the application.
2. Set your desired options.
3. Click **Generate**.
4. Use the **Copy to Clipboard** button as needed.

## Customization

- Edit the source code to add more character types or tweak UI/UX as desired.
- Adjust default values for password length, number to generate, or symbol sets in the `ReadSettings` procedure.

## License

*Specify your license here (e.g., MIT, GPL).*

## Author

- *Your Name or GitHub Username here*

---

*This project demonstrates password generation with user-friendly GUI and persistent settings using Lazarus/Free Pascal.*
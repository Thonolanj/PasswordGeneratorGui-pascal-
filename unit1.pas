unit unit1;

(*
  MIT License

  Copyright (c) 2025 [Danny Bush]

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*)

// This unit contains the main form and logic for a password generation utility.
// It allows users to specify password criteria, generate multiple passwords,
// and save their settings.

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  StdCtrls, Buttons, INIfiles, clipbrd, uAboutBox;

type

  { TForm1 }
  // Main form class for the Password Generator application.
  TForm1 = class(TForm)
    // --- UI Components ---
    Gen: TButton;
    CopyToClipboard: TButton;
    Button1: TButton; // 'Close' button
    PasswordLength: TSpinEdit;
    NumberToGen: TSpinEdit;
    Passwords: TListBox;
    btnAbout: TSpeedButton;
    UpCheck: TCheckBox;
    loCheck: TCheckBox;
    NumCheck: TCheckBox;
    FsCheck: TCheckBox;
    CSCheck: TCheckBox;
    loReq: TCheckBox;
    CustomSymbols: TEdit;
    EditCustomSymbols: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel; // For UpCheck
    Label4: TLabel;
    Label5: TLabel; // For loCheck
    Label6: TLabel; // For NumCheck
    Label7: TLabel; // For FsCheck
    Label8: TLabel; // For CSCheck
    Label9: TLabel; // For loReq

    // --- Event Handlers ---
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure GenClick(Sender: TObject);
    procedure CopyToClipboardClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure EditCustomSymbolsClick(Sender: TObject);

    // Event handlers for checkbox changes
    procedure loCheckChange(Sender: TObject);
    procedure loReqChange(Sender: TObject);
    procedure FsCheckChange(Sender: TObject);
    procedure CSCheckChange(Sender: TObject);

    // Event handlers to allow clicking labels to toggle checkboxes
    procedure Label3Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure PasswordsClick(Sender: TObject);
    procedure btnAboutClick(Sender: TObject);

  private
    // --- Private Fields ---
    FSymbols: string; // The active character set for symbols.

    // --- Private Methods ---
    procedure ReadSettings;
    procedure WriteSettings;
    procedure UpdateSymbolSet;
    function GetRandomChar(ACharType: Integer): Char;
    function ShuffleString(AInput: string): string;
    function GeneratePassword(ALength: Integer): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  // Character sets for password generation
  UPPERCASE_LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  LOWERCASE_LETTERS = 'abcdefghijklmnopqrstuvwxyz';
  NUMBERS = '0123456789';
  DEFAULT_SYMBOLS = '!@#$%^&*()[]{},.=-_+';

  // Constants for INI file configuration
  INI_FILE_NAME = 'PasswordGen.ini';
  INI_SECTION = 'General';

{ TForm1 }

//==============================================================================
// Form Event Handlers
//==============================================================================

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize the random number generator
  Randomize;
  // Load user settings from the INI file
  ReadSettings;
  // Set the initial symbol set based on loaded settings
  UpdateSymbolSet;
  // Set initial focus to the 'Generate' button for better UX
//  Gen.SetFocus;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // Save the current settings to the INI file before closing
  WriteSettings;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // The button labeled 'Close' will close the form
  Self.Close;
end;

//==============================================================================
// Core Functionality Event Handlers
//==============================================================================

procedure TForm1.GenClick(Sender: TObject);
var
  i: Integer;
begin
  // Ensure we have at least one character type selected
  if not (UpCheck.Checked or loCheck.Checked or NumCheck.Checked or
    FsCheck.Checked or CSCheck.Checked) then
  begin
    ShowMessage('Please select at least one character type to include.');
    Exit;
  end;

  Passwords.Items.Clear;
  for i := 1 to NumberToGen.Value do
  begin
    Passwords.Items.Add(GeneratePassword(PasswordLength.Value));
  end;

  // Automatically copy the first generated password to the clipboard
  if Passwords.Items.Count > 0 then
  begin
    Clipboard.AsText := Passwords.Items.Strings[0];
  end;
end;

procedure TForm1.CopyToClipboardClick(Sender: TObject);
var
  i: Integer;
  SelectedText: TStringList;
begin
  if Passwords.SelCount = 0 then
  begin
    // If nothing is selected, copy all generated passwords
    Clipboard.AsText := Passwords.Items.Text;
  end
  else
  begin
    // If items are selected, copy only the selected passwords
    SelectedText := TStringList.Create;
    try
      for i := 0 to Passwords.Items.Count - 1 do
      begin
        if Passwords.Selected[i] then
          SelectedText.Add(Passwords.Items[i]);
      end;
      Clipboard.AsText := SelectedText.Text;
    finally
      SelectedText.Free;
    end;
  end;
  // Deselect all items after copying
  Passwords.ClearSelection;
end;

procedure TForm1.PasswordsClick(Sender: TObject);
begin
  // Simple UX improvement: clicking any item in the listbox copies it.
  if Passwords.SelCount>0 then begin
    Clipboard.AsText := Passwords.GetSelectedText;
    Passwords.ClearSelection
  end;
end;

procedure TForm1.btnAboutClick(Sender: TObject);
  var
    AboutForm: TAboutBox;
  begin
    // Create an instance of the AboutBox form.
    AboutForm := TAboutBox.Create(nil);
    try
      // Call our new procedure to have the form populate itself
      // with the application's compiled-in information.
      AboutForm.LoadAppInfo;

      // Show the form modally.
      AboutForm.ShowModal;
    finally
      // Free the form's memory.
      AboutForm.Free;
    end;
end;

//==============================================================================
// UI Interaction and Settings Management
//==============================================================================

procedure TForm1.EditCustomSymbolsClick(Sender: TObject);
begin
  // Toggle the read-only state of the custom symbols text box
  CustomSymbols.ReadOnly := not CustomSymbols.ReadOnly;
  if CustomSymbols.ReadOnly then
  begin
    CustomSymbols.Color := clWindowFrame;
  end
  else
  begin
    CustomSymbols.Color := clGreen;
    CustomSymbols.SetFocus;
  end;
end;

procedure TForm1.CSCheckChange(Sender: TObject);
begin
  // Using custom symbols deselects the full symbol set
  if CSCheck.Checked and FsCheck.Checked then
    FsCheck.Checked := False;
  UpdateSymbolSet;
end;

procedure TForm1.FsCheckChange(Sender: TObject);
begin
  // Using the full symbol set deselects custom symbols
  if FsCheck.Checked and CSCheck.Checked then
    CSCheck.Checked := False;
  UpdateSymbolSet;
end;

procedure TForm1.loCheckChange(Sender: TObject);
begin
  // If lowercase is disabled, the 'first char must be lowercase' option
  // must also be disabled.
  if not loCheck.Checked then
    loReq.Checked := False;
end;

procedure TForm1.loReqChange(Sender: TObject);
begin
  // If 'first char must be lowercase' is enabled, the main lowercase
  // option must also be enabled.
  if loReq.Checked then
    loCheck.Checked := True;
end;

// -- The following handlers allow clicking on a label to toggle its associated checkbox --
procedure TForm1.Label3Click(Sender: TObject);
begin
  UpCheck.Checked := not UpCheck.Checked;
end;

procedure TForm1.Label5Click(Sender: TObject);
begin
  loCheck.Checked := not loCheck.Checked;
end;

procedure TForm1.Label6Click(Sender: TObject);
begin
  NumCheck.Checked := not NumCheck.Checked;
end;

procedure TForm1.Label7Click(Sender: TObject);
begin
  FsCheck.Checked := not FsCheck.Checked;
end;

procedure TForm1.Label8Click(Sender: TObject);
begin
  CSCheck.Checked := not CSCheck.Checked;
end;

procedure TForm1.Label9Click(Sender: TObject);
begin
  loReq.Checked := not loReq.Checked;
end;

//==============================================================================
// Private Helper Methods
//==============================================================================

procedure TForm1.UpdateSymbolSet;
begin
  // This helper centralizes the logic for determining which symbol set to use.
  if FsCheck.Checked then
    FSymbols := DEFAULT_SYMBOLS
  else if CSCheck.Checked then
    FSymbols := CustomSymbols.Text
  else
    FSymbols := ''; // No symbols
end;

procedure TForm1.WriteSettings;
var
  INI: TINIFile;
begin
  // Saves all relevant UI settings to an INI file in the user's home directory.
  INI := TINIFile.Create(GetUserDir + INI_FILE_NAME);
  try
    INI.WriteBool(INI_SECTION, 'UpperCase', UpCheck.Checked);
    INI.WriteBool(INI_SECTION, 'LowerCase', loCheck.Checked);
    INI.WriteBool(INI_SECTION, 'Numbers', NumCheck.Checked);
    INI.WriteBool(INI_SECTION, 'FullSymbols', FsCheck.Checked);
    INI.WriteBool(INI_SECTION, 'CustomSymbols', CSCheck.Checked);
    INI.WriteBool(INI_SECTION, 'FirstCharLower', loReq.Checked);
    INI.WriteInteger(INI_SECTION, 'PasswordLength', PasswordLength.Value);
    INI.WriteInteger(INI_SECTION, 'NumberToGen', NumberToGen.Value);
    INI.WriteString(INI_SECTION, 'CustomSymbolsText', CustomSymbols.Text);
  finally
    INI.Free;
  end;
end;

procedure TForm1.ReadSettings;
var
  INI: TINIFile;
begin
  // Reads settings from an INI file and applies them to the UI, using
  // sensible defaults if the file or a setting doesn't exist.
  INI := TINIFile.Create(GetUserDir + INI_FILE_NAME);
  try
    UpCheck.Checked := INI.ReadBool(INI_SECTION, 'UpperCase', True);
    loCheck.Checked := INI.ReadBool(INI_SECTION, 'LowerCase', True);
    NumCheck.Checked := INI.ReadBool(INI_SECTION, 'Numbers', True);
    FsCheck.Checked := INI.ReadBool(INI_SECTION, 'FullSymbols', False);
    CSCheck.Checked := INI.ReadBool(INI_SECTION, 'CustomSymbols', True);
    loReq.Checked := INI.ReadBool(INI_SECTION, 'FirstCharLower', False);
    PasswordLength.Value := INI.ReadInteger(INI_SECTION, 'PasswordLength', 16);
    NumberToGen.Value := INI.ReadInteger(INI_SECTION, 'NumberToGen', 1);
    CustomSymbols.Text := INI.ReadString(INI_SECTION, 'CustomSymbolsText', '@#*-_?');
  finally
    INI.Free;
  end;
end;

function TForm1.GetRandomChar(ACharType: Integer): Char;
begin
  // Returns a single random character based on the requested type.
  // 1=Upper, 2=Lower, 3=Number, 4=Symbol
  case ACharType of
    1: Result := UPPERCASE_LETTERS[Random(Length(UPPERCASE_LETTERS)) + 1];
    2: Result := LOWERCASE_LETTERS[Random(Length(LOWERCASE_LETTERS)) + 1];
    3: Result := NUMBERS[Random(Length(NUMBERS)) + 1];
    4: if Length(FSymbols) > 0 then
         Result := FSymbols[Random(Length(FSymbols)) + 1]
       else
         Result := Char(0); // Return null char if symbol set is empty
  else
    Result := Char(0); // Should not happen
  end;
end;

function TForm1.ShuffleString(AInput: string): string;
var
  i, j: Integer;
  ch: Char;
  StartIndex: Integer;
begin
  Result := AInput;
  StartIndex := 1;

  // If the first character must be lowercase, we don't shuffle it.
  if (Length(Result) > 1) and (loReq.Checked) then
  begin
    StartIndex := 2;
  end;

  // Perform a Fisher-Yates shuffle on the relevant part of the string.
  for i := Length(Result) downto StartIndex + 1 do
  begin
    j := Random(i - StartIndex + 1) + StartIndex;
    if i <> j then
    begin
      ch := Result[i];
      Result[i] := Result[j];
      Result[j] := ch;
    end;
  end;
end;

function TForm1.GeneratePassword(ALength: Integer): string;
var
  Password: string;
  GuaranteedCharsCount: Integer;
  i, RandomType: Integer;
  IsValidType: Boolean;
begin
  Password := '';
  SetLength(Password, ALength);
  GuaranteedCharsCount := 0;

  // --- Step 1: Ensure at least one of each selected character type exists ---
  // This makes the generated password stronger and meets complexity requirements.

  // If 'First char is lowercase' is required, handle it first.
  if loReq.Checked then
  begin
    Inc(GuaranteedCharsCount);
    Password[GuaranteedCharsCount] := GetRandomChar(2); // 2 = Lowercase
  end;
  // If lowercase is selected but not required as the first char.
  if loCheck.Checked and not loReq.Checked then
  begin
    Inc(GuaranteedCharsCount);
    Password[GuaranteedCharsCount] := GetRandomChar(2);
  end;
  // Add an uppercase character if selected.
  if UpCheck.Checked then
  begin
    Inc(GuaranteedCharsCount);
    Password[GuaranteedCharsCount] := GetRandomChar(1); // 1 = Uppercase
  end;
  // Add a number if selected.
  if NumCheck.Checked then
  begin
    Inc(GuaranteedCharsCount);
    Password[GuaranteedCharsCount] := GetRandomChar(3); // 3 = Number
  end;
  // Add a symbol if any symbol set is selected.
  if (FsCheck.Checked or CSCheck.Checked) and (Length(FSymbols) > 0) then
  begin
    Inc(GuaranteedCharsCount);
    Password[GuaranteedCharsCount] := GetRandomChar(4); // 4 = Symbol
  end;

  // --- Step 2: Fill the rest of the password with random chars ---
  for i := GuaranteedCharsCount + 1 to ALength do
  begin
    // Loop until we pick a valid, selected character type.
    repeat
      RandomType := Random(4) + 1; // Pick a type from 1 to 4
      IsValidType := False;
      case RandomType of
        1: IsValidType := UpCheck.Checked;
        2: IsValidType := loCheck.Checked;
        3: IsValidType := NumCheck.Checked;
        4: IsValidType := (FsCheck.Checked or CSCheck.Checked) and (Length(FSymbols) > 0);
      end;
    until IsValidType;
    Password[i] := GetRandomChar(RandomType);
  end;

  // --- Step 3: Shuffle the generated string ---
  // This mixes the guaranteed characters randomly throughout the password.
  Result := ShuffleString(Password);
end;

end.


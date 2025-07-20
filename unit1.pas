unit Unit1;

{$mode objfpc}{$H+}



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  StdCtrls, Buttons, INIfiles, clipbrd;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CopyToClipboard: TButton;
    CSCheck: TCheckBox;
    CustomSymbols: TEdit;
    EditCustomSymbols: TBitBtn;
    FsCheck: TCheckBox;
    Gen: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    loCheck: TCheckBox;
    loReq: TCheckBox;
    NumberToGen: TSpinEdit;
    NumCheck: TCheckBox;
    PasswordLength: TSpinEdit;
    Passwords: TListBox;
    UpCheck: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure CopyToClipboardClick(Sender: TObject);
    procedure CSCheckChange(Sender: TObject);
    procedure EditCustomSymbolsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FsCheckChange(Sender: TObject);
    procedure GenClick(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure loCheckChange(Sender: TObject);
    procedure loReqChange(Sender: TObject);
    procedure PasswordsClick(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

const
  upletters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  lowLetters = 'abcdefghijklmnopqrstuvwxyz';
  numbers = '0123456789';
  symbols = '!@#$%^&*()[]{},.=-_+';

var
  Syms: String;


procedure WriteSettings;
  var
    INI: TINIFile;
    sect: String;


  begin
    sect := 'General';
    INI := TINIFile.Create(GetUserDir + 'PasswordGen.ini');
    INI.WriteBool(sect, 'UpperCase', form1.UpCheck.Checked);
    INI.WriteBool(sect, 'LowerCase', form1.loCheck.Checked);
    INI.WriteBool(sect, 'Numbers', form1.NumCheck.Checked);
    INI.WriteBool(sect, 'FullSymbols', form1.FsCheck.Checked);
    INI.WriteBool(sect, 'CustomSymbols', form1.CSCheck.Checked);
    INI.WriteBool(sect, 'FirstCharLower', form1.loReq.Checked);
    INI.WriteInteger(sect, 'PasswordLength', form1.PasswordLength.Value);
    INI.WriteInteger(sect, 'NumberToGen', form1.NumberToGen.Value);
    INI.WriteString(sect, 'CustomSymbolsText', form1.CustomSymbols.Text);
    INI.Free;

  end;

procedure ReadSettings;
  var
    INI: TINIFile;
    sect: String;

  begin
    sect := 'General';
    INI := TINIFile.Create(GetUserDir + 'PasswordGen.ini');
    form1.UpCheck.Checked := INI.ReadBool(sect, 'UpperCase', True);
    form1.loCheck.Checked := INI.ReadBool(sect, 'LowerCase', True);
    form1.NumCheck.Checked := INI.ReadBool(sect, 'Numbers', True);
    form1.FsCheck.Checked := INI.ReadBool(sect, 'FullSymbols', False);
    form1.CSCheck.Checked := INI.ReadBool(sect, 'CustomSymbols', True);
    form1.loReq.Checked := INI.ReadBool(sect, 'FirstCharLower', False);
    form1.PasswordLength.Value := INI.ReadInteger(sect, 'PasswordLength', 16);
    form1.NumberToGen.Value := INI.ReadInteger(sect, 'NumberToGen', 1);
    form1.CustomSymbols.Text := INI.ReadString(sect, 'CustomSymbolsText', '@#*-_?');

    INI.Free;
  end;


function getchar(chartype: Integer): Char;
  begin
    case chartype of
      1: Result := upletters[random(length(upletters)) + 1];
      2: Result := lowletters[random(length(lowletters)) + 1];
      3: Result := numbers[random(length(numbers)) + 1];
      4: Result := syms[random(length(syms)) + 1];
      else
        Result := Char('');
    end;
  end;

function Shuffle(inp: String): String;
  var
    x, ReplacementDigit: Longint;
    ch: Char;
    FirstLo: integer;
    offset : integer;

  begin
    FirstLo := 1;
    offset :=1;
    if length(inp) > 1 then
    if Form1.loReq.checked then begin
      inc(FirstLo);
      inc(offset);
    end;
    begin
      for x := FirstLo to length(inp) do
      begin
        repeat
          ReplacementDigit := random(length(inp)-(offset-1)) + offset;
        until (ReplacementDigit <> x);
        ch := inp[x];
        inp[x] := inp[ReplacementDigit];
        inp[ReplacementDigit] := ch;
      end;
    end;
    Result := inp;
  end;

function GenPW(size: Integer): String;
  var
    pw: String;
    counter: Integer;
    skip: Integer;
    i: Integer;
    Valid: Boolean;
  begin
    pw := '';
    skip := 1;
    I := 1;
    setlength(pw, size);
    if Form1.loCheck.Checked or Form1.loReq.Checked then
    begin
      pw[skip] := getchar(2);
      Inc(skip);
    end;
    if form1.UpCheck.Checked then
    begin
      pw[skip] := getchar(1);
      Inc(skip);
    end;
    if Form1.NumCheck.Checked then
    begin
      pw[skip] := getchar(3);
      Inc(skip);
    end;
    if Form1.FsCheck.Checked or Form1.CSCheck.Checked then
    begin
      pw[skip] := getchar(4);
      Inc(skip);
    end;
    for counter := skip to size do
    begin
      valid := False;
      repeat
        i := random(4);
        case i of
          0: if form1.UpCheck.Checked then
              Valid := True;
          1: if form1.loCheck.Checked then
              Valid := True;
          2: if form1.NumCheck.Checked then
              Valid := True;
          3: if form1.FsCheck.Checked or form1.CSCheck.Checked then
              Valid := True;
        end;
      until valid;
      pw[counter] := getchar(i + 1);
    end;
    pw := Shuffle(pw);
    Result := pw;
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
    randomize;
    ReadSettings;
    Syms := CustomSymbols.Text;
    if FsCheck.Checked then
      Syms := symbols;
    gen.SetFocus;

  end;

procedure TForm1.FsCheckChange(Sender: TObject);
  begin

    if FsCheck.Checked then
      if CSCheck.Checked then
        CSCheck.Checked := False;
    if FsCheck.Checked then
      Syms := symbols;

  end;


procedure TForm1.EditCustomSymbolsClick(Sender: TObject);
  begin
    CustomSymbols.ReadOnly := not (CustomSymbols.ReadOnly);
    if CustomSymbols.ReadOnly then
      CustomSymbols.Color := clWindowFrame
    else
      CustomSymbols.Color := clGreen;
    CustomSymbols.SetFocus;
  end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    WriteSettings;
  end;

procedure TForm1.CopyToClipboardClick(Sender: TObject);
  begin
    if Passwords.SelCount=0 then
      Clipboard.AsText := Passwords.Items.Text
    else
      Clipboard.AsText := Passwords.GetSelectedText;
    Passwords.ClearSelection;
  end;

procedure TForm1.CSCheckChange(Sender: TObject);
  begin
    if CSCheck.Checked then
      if FsCheck.Checked then
        FsCheck.Checked := False;
    if CSCheck.Checked then
      Syms := CustomSymbols.Text;
  end;

procedure TForm1.Button1Click(Sender: TObject);
  begin
    form1.Close;
  end;

procedure TForm1.GenClick(Sender: TObject);
  var
    i: Integer;
  begin
    Passwords.Items.Clear;
    for i := 1 to NumberToGen.Value do
      Passwords.Items.AddText(GenPW(PasswordLength.Value));
    Clipboard.AsText := Passwords.items.Strings[0];
  end;

procedure TForm1.Label3Click(Sender: TObject);
  begin
    UpCheck.Checked := not (UpCheck.Checked);
  end;

procedure TForm1.Label5Click(Sender: TObject);
  begin
    loCheck.Checked := not (loCheck.Checked);

  end;

procedure TForm1.Label6Click(Sender: TObject);
  begin
    NumCheck.Checked := not (NumCheck.Checked);

  end;

procedure TForm1.Label7Click(Sender: TObject);
  begin
    FsCheck.Checked := not (FsCheck.Checked);
  end;

procedure TForm1.Label8Click(Sender: TObject);
  begin
    CSCheck.Checked := not (CSCheck.Checked);

  end;

procedure TForm1.Label9Click(Sender: TObject);
begin
  loReq.Checked:= not (loReq.Checked);
end;

procedure TForm1.loCheckChange(Sender: TObject);
begin
  if not(loCheck.Checked) then loReq.Checked := false;
end;

procedure TForm1.loReqChange(Sender: TObject);
begin
  if loReq.Checked then loCheck.Checked:= true;
end;

procedure TForm1.PasswordsClick(Sender: TObject);
begin
  if Passwords.SelCount>0 then
    Passwords.ClearSelection
  else
    Clipboard.AsText := Passwords.GetSelectedText;
end;









end.

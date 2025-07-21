unit uAboutBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  fileinfo, math, lclintf;

type

  { TAboutBox }

  TAboutBox = class(TForm)
    btnOK: TButton;
    Button1: TButton;
    imgIcon: TImage;
    Label1: TLabel;
    lblAppName: TLabel;
    lblVersion: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure imgIconClick(Sender: TObject);
  private

  public
    procedure LoadAppInfo;
  end;

var
  AboutBox: TAboutBox;

implementation

const
  // Define the MIT License text as a constant.
  // sLineBreak is a system constant for a new line, making the text portable.
  MIT_LICENSE_TEXT =
    'MIT License' + sLineBreak + sLineBreak +
    'Copyright (c) 2025 [Danny Bush]' + sLineBreak + sLineBreak +
    'Permission is hereby granted, free of charge, to any person obtaining a copy ' +
    'of this software and associated documentation files (the "Software"), to deal ' +
    'in the Software without restriction, including without limitation the rights ' +
    'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell ' +
    'copies of the Software, and to permit persons to whom the Software is ' +
    'furnished to do so, subject to the following conditions:' + sLineBreak + sLineBreak +
    'The above copyright notice and this permission notice shall be included in all ' +
    'copies or substantial portions of the Software.' + sLineBreak + sLineBreak +
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR ' +
    'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, ' +
    'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE ' +
    'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER ' +
    'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ' +
    'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ' +
    'SOFTWARE.';


{$R *.lfm}

procedure TAboutBox.Button1Click(Sender: TObject);

  var
  // Dialog will be the form for our message box.
  Dialog: TForm;
  // Memo will be the scrollable text area.
  Memo: TMemo;
begin
  // CreateMessageDialog is a powerful LCL function that creates a standard
  // dialog form for us, complete with an OK button.
  // We pass it a caption and a placeholder message (which we will ignore).
  Dialog := CreateMessageDialog('License Information', '', mtInformation, [mbOK]);
  try
    // Set a good size for our dialog box.
    Dialog.Width := 600;
    Dialog.Height := 500;
    // Ensure it appears in the center of the screen.
    Dialog.Position := poScreenCenter;

    // Create a TMemo component dynamically. We set its owner to 'Dialog'
    // so that when the dialog is freed, the memo is automatically freed too.
    Memo := TMemo.Create(Dialog);
    Memo.Parent := Dialog; // Make the memo visible on the dialog.
    Memo.Font.Size := Max(12, Memo.Font.Size);
    // Configure the memo to be perfect for displaying a license.
    Memo.ReadOnly := True;
    Memo.ScrollBars := ssVertical;
    Memo.WordWrap := True;

    // This is the key: set its alignment to alClient. This makes the memo
    // expand to fill all the available space on the dialog, leaving room
    // for the button panel at the bottom that CreateMessageDialog made for us.
    Memo.Align := alClient;

    // Load our license text into the memo.
    Memo.Lines.Text := MIT_LICENSE_TEXT;

    // Finally, show the dialog modally. Execution will pause here until
    // the user clicks the "OK" button.
    Dialog.ShowModal;
  finally
    // CreateMessageDialog returns a form that we are responsible for freeing.
    // The 'finally' block ensures this happens even if an error occurs.
    Dialog.Free;
  end;
end;

procedure TAboutBox.imgIconClick(Sender: TObject);
begin
  OpenURL('https://www.iconshock.com/icons-pack/security-icons/password-generator-icon/');
end;

procedure TAboutBox.LoadAppInfo;

var
  versioninfo: string;
  VQ : TVersionQuad;


begin
  versioninfo := '';
  // ParamStr(0) is a system function that returns the full path
  // to the currently running executable file.
    With TFileVersionInfo.Create(Nil) do
       try
         FileName:=ParamStr(0);
         Translation:='123';
         Filter.Add('Fileversion');
         Enabled:=True;
       Finally
         Free;
       end;
    if GetProgramVersion(VQ) then versioninfo := versionQuadToStr(VQ);

    lblAppName.Caption := 'Password Generator';

    // Set the version label.
    lblVersion.Caption := 'Version ' + versioninfo;

    // Load the main application icon from the executable's resources.
//    imgIcon.Picture.Icon.LoadFromResourceName(HInstance, 'MAINICON');

end;


end.

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
  Dialog: TForm;
  Memo: TMemo;
  BottomPanel: TPanel;
  OKButton: TButton;
begin
  // Create a new, blank form to act as our dialog.
  Dialog := TForm.Create(nil);
  try
    // --- 1. Configure the Main Dialog Form ---
    Dialog.Caption := 'License Information';
    Dialog.Width := 650;
    Dialog.Height := 650;
    Dialog.Position := poScreenCenter;
    Dialog.BorderStyle := bsDialog;

    // --- 2. Create a Panel at the bottom to hold our button ---
    BottomPanel := TPanel.Create(Dialog);
    BottomPanel.Parent := Dialog;
    BottomPanel.Align := alBottom; // Snap it to the bottom of the form.
    BottomPanel.Height := 40;     // Give it a fixed height.
    BottomPanel.BevelOuter := bvNone; // Make its border invisible for a clean look.
    BottomPanel.Caption := '';

    // --- 3. Create the OK button and place it on the panel ---
    OKButton := TButton.Create(Dialog);
    OKButton.Parent := BottomPanel; // Place the button ON the panel.
    OKButton.Caption := 'OK';
    OKButton.ModalResult := mrOK; // This makes it automatically close the dialog.
    OKButton.Anchors := [akTop, akRight]; // Keep it anchored to the top-right of the panel.
    // Position it with a nice margin from the edge.
    OKButton.SetBounds(BottomPanel.Width - 85, 8, 75, 25);

    // --- 4. Create the Memo to fill the rest of the space ---
    Memo := TMemo.Create(Dialog);
    Memo.Parent := Dialog;
    // Align to client will now fill the space NOT occupied by BottomPanel.
    Memo.Align := alClient;
    Memo.ReadOnly := True;
    Memo.ScrollBars := ssVertical;
    Memo.WordWrap := True;
    Memo.Font.Size := Max(14, Memo.Font.Size); // Set min font size.

    // --- 5. Load the text and show the dialog ---
    Memo.Lines.Text := MIT_LICENSE_TEXT;
    Dialog.ShowModal;

  finally
    // Free the dialog and all its child components from memory.
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

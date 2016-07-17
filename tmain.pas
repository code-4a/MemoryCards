unit TMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Menus, PairSplitter, Cards, Nodes,
  AppConfig, types, FormEditCsv;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnTranslate: TButton;
    btnNext: TButton;
    lblCardsCounter: TLabel;
    mnuTreeCreateFolder: TMenuItem;
    mnuTreeCreate: TMenuItem;
    mnuTreeCreateFile: TMenuItem;
    mnuTreeEditFile: TMenuItem;
    mnuAppKeys: TMenuItem;
    mnuApp: TMenuItem;
    mnuPackReverse: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuPack: TMenuItem;
    mnuPackMix: TMenuItem;
    MMenu: TMainMenu;
    nmuFile: TMenuItem;
    mnuTree: TPopupMenu;
    splSplitterCont: TPairSplitter;
    splSplitter1: TPairSplitterSide;
    splSplitter2: TPairSplitterSide;
    pnlText1: TPanel;
    pnlText2: TPanel;
    ProgressBar1: TProgressBar;
    trvFiles: TTreeView;
    procedure btnNextClick(Sender: TObject);
    procedure btnTranslateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuPackMixClick(Sender: TObject);
    procedure mnuPackReverseClick(Sender: TObject);
    procedure mnuTreeEditFileClick(Sender: TObject);
    procedure splSplitter2Resize(Sender: TObject);
    procedure trvFilesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure trvFilesDblClick(Sender: TObject);
  private
    { private declarations }
    Config: TAppConfig;
    Cards: TCards;
    Nodes: TNodes;
    frmEditFile: TfrmEditCsv;
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  root: TTreeNode;
begin
  Cards := TCards.Create;
  Nodes := TNodes.Create;
  Config := TAppConfig.Create;
  Config.GetControlSize(frmMain, 'MainForm');
  Config.GetControlColor(pnlText1, 'TopPanel');
  Config.GetControlColor(pnlText2, 'BottomPanel');
  root := trvFiles.Items.Add(nil, 'Cards');
  Nodes.GetTreeFiles(trvFiles, root, Config.AppPath + 'Cards');
  frmMain.KeyPreview := True;
  frmEditFile := TfrmEditCsv.Create(nil);
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (Key < 65) and (Key > 122) then
    Exit;
  Case Chr(Key) of
    'N': btnNextClick(Sender);
    'V': btnTranslateClick(Sender);
    'R': mnuPackReverseClick(Sender);
  end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin

end;

procedure TfrmMain.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuPackMixClick(Sender: TObject);
begin
  Cards.Mix;
  lblCardsCounter.Caption := '1 / ' + IntToStr(Cards.Count);
  ProgressBar1.Position := 1;
  pnlText1.Caption := Cards.Term;
  pnlText2.Caption := '';
end;

procedure TfrmMain.mnuPackReverseClick(Sender: TObject);
begin
  mnuPackReverse.Checked :=  not mnuPackReverse.Checked;
  Cards.Reverse := mnuPackReverse.Checked;
  pnlText1.Caption := Cards.Term;
  pnlText2.Caption := '';
end;

procedure TfrmMain.mnuTreeEditFileClick(Sender: TObject);
var
  s: String;
begin
  if trvFiles.Selected = nil then Exit;
  s := Nodes.GetFilePath(trvFiles.Selected.AbsoluteIndex);
  if s <> '' then
  begin
    frmEditFile.edtFileName.Text := ExtractFileNameWithoutExt(ExtractFileName(s));
    frmEditFile.stgWords.LoadFromCSVFile(s, ';', false);
    if frmEditFile.ShowModal = 1 then
    begin

    end;
  end;
end;

procedure TfrmMain.splSplitter2Resize(Sender: TObject);
var
  i: integer;
begin
  i := splSplitter2.Height - (pnlText1.Height + pnlText2.Height + 38);
  if i <> 0 then
  begin
    i := i div 2;
    pnlText1.Height := pnlText1.Height + i;
    pnlText2.Height := pnlText2.Height + i;
    pnlText2.Top := pnlText1.Top + pnlText1.Height + 2;
  end;
end;

procedure TfrmMain.trvFilesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  s: String;
  ti: TMenuItem;
begin
  if trvFiles.Selected = nil then Exit;
  s := Nodes.GetFilePath(trvFiles.Selected.AbsoluteIndex);
  for ti in mnuTree.Items do
    if s <> '' then
      //todo: filtering menu items

end;

procedure TfrmMain.trvFilesDblClick(Sender: TObject);
var
  s: String;
begin
   if trvFiles.Selected <> nil then
   begin
     s := Nodes.GetFilePath(trvFiles.Selected.AbsoluteIndex);
     if s <> '' Then
     begin
       Cards.LoadFile(s);
       lblCardsCounter.Caption := '1 / ' + IntToStr(Cards.Count);
       ProgressBar1.Max := Cards.Count;
       ProgressBar1.Position := 1;
       pnlText1.Caption := Cards.Term;
       pnlText2.Caption := '';
     end;
   end;
end;

procedure TfrmMain.btnNextClick(Sender: TObject);
begin
  Cards.Next;
  pnlText1.Caption := Cards.Term;
  pnlText2.Caption := '';
  lblCardsCounter.Caption := IntToStr(Cards.CurrentID + 1) + ' / ' + IntToStr(Cards.Count);
  ProgressBar1.Position := Cards.CurrentID + 1;
end;

procedure TfrmMain.btnTranslateClick(Sender: TObject);
begin
  pnlText2.Caption := Cards.Value;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.SetControlSize(frmMain, 'MainForm');
  Config.SetControlColor(pnlText1, 'TopPanel');
  Config.SetControlColor(pnlText2, 'BottomPanel');
  Config.SaveConfig;
end;


end.


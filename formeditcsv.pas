unit FormEditCsv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls;

type

  { TfrmEditCsv }

  TfrmEditCsv = class(TForm)
    btnSave: TButton;
    btnClose: TButton;
    edtFileName: TEdit;
    Label1: TLabel;
    stgWords: TStringGrid;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmEditCsv: TfrmEditCsv;

implementation

{$R *.lfm}

{ TfrmEditCsv }



end.


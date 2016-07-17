unit nodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FileUtil;

type

  { TNodes }

  TNodes = class
    private
      Paths: TStringList;
    public
      Constructor Create;
      procedure GetTreeFiles(trv: TTreeView; node: TTreeNode; path: String);
      function GetFilePath(Index: integer): String;
  end;

implementation

{ TNodes }

constructor TNodes.Create;
begin
  Paths := TStringList.Create;
end;

procedure TNodes.GetTreeFiles(trv: TTreeView; node: TTreeNode; path: String);
var
  newNode: TTreeNode;
  s: String;
  f: String;
begin
  for s in FindAllDirectories(path, False) do
  begin
    f := ExtractFileName(s);
    newNode := trv.Items.AddChild(node, f);
    Paths.Append('');
    GetTreeFiles(trv, newNode, AppendPathDelim(path) + f);
  end;
  for s in FindAllFiles(path, '*.*', False) do
  begin
    newNode := trv.Items.AddChild(node, ExtractFileName(s));
    Paths.Append(s);
  end;
end;

function TNodes.GetFilePath(Index: integer): String;
begin
  if (Index <= Paths.Count) and (Index > 0) then
    result := Paths.Strings[Index - 1]
  else
    result := '';
end;

end.


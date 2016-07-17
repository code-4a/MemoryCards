unit Cards;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, KeyValueFile;

type

  { TCards }
  TCards = class
    private
      CurrentLine: integer;
      CardSet: TKeyValueFile;
      isReverse: boolean;
      function GetText1: String;
      function GetText2: String;
    public
      constructor Create;
      property Term: String read GetText1;
      property Value: String read GetText2;
      property Reverse: boolean read isReverse write isReverse;
      property CurrentID: Integer read CurrentLine;
      procedure LoadFile(Path: String);
      procedure Next;
      procedure Mix;
      function Count: integer;
  end;

implementation

function TCards.GetText1: String;
begin
  if CurrentLine < 0 Then
    Exit;
  if isReverse then
    result := CardSet.GetData(CurrentLine).Value
  else
    result := CardSet.GetData(CurrentLine).Key;
end;

function TCards.GetText2: String;
begin
   if CurrentLine < 0 Then
     Exit;
   if isReverse then
    result := CardSet.GetData(CurrentLine).Key
  else
    result := CardSet.GetData(CurrentLine).Value;
end;

constructor TCards.Create;
begin
    CurrentLine := -1;
    CardSet := TKeyValueFile.Create;
end;

procedure TCards.LoadFile(Path: String);
begin
  CardSet.LoadFile(Path, ';');
  CurrentLine := 0;
end;

function TCards.Count: integer;
begin
  Result := CardSet.Count;
end;

procedure TCards.Next;
begin
  if CardSet.Count <= 0 then Exit;
  CurrentLine := CurrentLine + 1;
  if CurrentLine > (CardSet.Count - 1) then
    CurrentLine := 0;
end;

procedure TCards.Mix;
var
  new: Longint;
  i: integer;
  card: TKeyValueRec;
  CCount: integer;
begin
  Randomize;
  CCount := CardSet.Count;
  for i := 0 to CCount - 1 do
  begin
    new := Random(CCount);
    if i <> new then
      card := CardSet.GetData(i);
      CardSet.SetData(i, CardSet.GetData(new));
      CardSet.SetData(new, card);
  end;
  CurrentLine := -1;
  Next;
end;

end.


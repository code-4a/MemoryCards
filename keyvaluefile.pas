unit KeyValueFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

type
  TKeyValueRec = record
    Key:   String;
    Value: String;
  end;

  { TKeyValueFile }

  TKeyValueFile = class
    private
      FilePath:   String;
      Separator:  String;
      arrKeyValue: array of TKeyValueRec;
      function  StrToKeyValue(s: String): TKeyValueRec;
      function  FindID(key: String): integer;
      procedure NewKeyValuePair(key, value: String);
    public
      constructor Create;
      procedure LoadFile(path: String; sep: String);
      procedure SaveFile;
      procedure SetValue(key: String; value: String); overload;
      procedure SetValue(key: String; value: Integer); overload;
      function GetValue(key: String; defValue: String): String; overload;
      function GetValue(key: String; defValue: integer): integer; overload;
      function GetData(id: integer): TKeyValueRec;
      procedure SetData(id: integer; kv: TKeyValueRec);
      function Count: integer;
  end;

implementation

{ TKeyValueFile }

function TKeyValueFile.StrToKeyValue(s: String): TKeyValueRec;
var
  kv: TKeyValueRec;
  iPos: integer;
begin
  iPos := PosEx(Separator, s);
  if iPos > 0 Then
  begin
    kv.Key := Trim(MidStr(s, 0, iPos - 1));
    kv.Value := Trim(MidStr(s, iPos + 1, Length(s) - 1));
  end;
  result := kv;
end;

function TKeyValueFile.FindID(key: String): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to High(arrKeyValue) do
    if arrKeyValue[i].Key = key then
    begin
      result := i;
      Exit;
    end;
end;

procedure TKeyValueFile.NewKeyValuePair(key, value: String);
begin
  SetLength(arrKeyValue, High(arrKeyValue) + 2);
  arrKeyValue[High(arrKeyValue)].Key := key;
  arrKeyValue[High(arrKeyValue)].Value := value;
end;


constructor TKeyValueFile.Create;
begin
  //
end;

procedure TKeyValueFile.LoadFile(path: String; sep: String);
var
  st: TStringList;
  i: integer;
begin
  FilePath := path;
  Separator := sep;
  if FileExists(FilePath) then
  begin
    st := TStringList.Create;
    st.LoadFromFile(FilePath);
    SetLength(arrKeyValue, st.Count);
    for i:= 0 to st.Count - 1 do
    begin
      arrKeyValue[i] := StrToKeyValue(st.Strings[i]);
    end;
    st.Free;
  end;
end;

procedure TKeyValueFile.SaveFile;
var
  i: integer;
  st: TStringList;
begin
  st := TStringList.Create;
  for i := 0 to High(arrKeyValue) do
    st.Add(arrKeyValue[i].Key + ' = ' + arrKeyValue[i].Value);
  st.SaveToFile(FilePath);
  st.Free;
end;

procedure TKeyValueFile.SetValue(key: String; value: String);
var
  i: Integer;
begin
  i := FindID(key);
  if i < 0 then
    NewKeyValuePair(key, value)
  else
    arrKeyValue[i].Value := value;
end;

procedure TKeyValueFile.SetValue(key: String; value: Integer);
begin
  SetValue(key, IntToStr(value));
end;

function TKeyValueFile.GetValue(key: String; defValue: String): String;
var
  i: integer;
begin
  i := FindID(key);
  if i < 0 then
    result := defValue
  else
    result := arrKeyValue[i].Value;
end;

function TKeyValueFile.GetValue(key: String; defValue: integer): integer;
begin
  result := StrToIntDef(GetValue(key, ''), defValue);
end;

function TKeyValueFile.GetData(id: integer): TKeyValueRec;
var
  kv: TKeyValueRec;
begin
  if (id >= 0) and (id <= High(arrKeyValue)) then
    result := arrKeyValue[id]
  else
    result := kv;
end;

procedure TKeyValueFile.SetData(id: integer; kv: TKeyValueRec);
begin
  if (id >= 0) and (id <= High(arrKeyValue)) then
  begin
    arrKeyValue[id] := kv;
  end;
end;

function TKeyValueFile.Count: integer;
begin
  result := High(arrKeyValue) + 1;
end;

end.


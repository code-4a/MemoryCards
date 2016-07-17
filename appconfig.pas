unit AppConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, forms, KeyValueFile, Graphics;

type
  { TAppConfig }

  TAppConfig = class
  private
    MyFolderPath: String;
    ConfigPath: String;
    KeyValue: TKeyValueFile;
  public
    constructor Create;
    property AppPath: String read MyFolderPath;
    procedure GetControlSize(Control: TControl; Name: String);
    procedure SetControlSize(Control: TControl; Name: String);
    procedure GetControlColor(Control: TControl; Name: String);
    procedure SetControlColor(Control: TControl; Name: String);
    procedure GetFontInfo(Font: TFont; Name: String);
    procedure SetFontInfo(Font: TFont; Name: String);
    procedure SaveConfig;
  end;

implementation

{ TAppConfig }

constructor TAppConfig.Create;
begin
  MyFolderPath := AppendPathDelim(ExtractFileDir(Application.ExeName));
  ConfigPath := MyFolderPath + 'MemoryCards.conf';
  KeyValue := TKeyValueFile.Create;
  KeyValue.LoadFile(ConfigPath, '=');
end;

procedure TAppConfig.GetControlSize(Control: TControl; Name: String);
begin
  Control.Height := KeyValue.GetValue(Name + '.Height', Control.Height);
  Control.Width  := KeyValue.GetValue(Name + '.Width', Control.Width);
end;

procedure TAppConfig.SetControlSize(Control: TControl; Name: String);
begin
  KeyValue.SetValue(Name + '.Height', Control.Height);
  KeyValue.SetValue(Name + '.Width', Control.Width);
end;

procedure TAppConfig.GetControlColor(Control: TControl; Name: String);
begin
  Control.Color := KeyValue.GetValue(Name + '.Color', Control.Color);
end;

procedure TAppConfig.SetControlColor(Control: TControl; Name: String);
begin
  KeyValue.SetValue(Name + '.Color', Control.Color);
end;

procedure TAppConfig.GetFontInfo(Font: TFont; Name: String);
begin
  Font.Size := KeyValue.GetValue(Name + '.FontSize', Font.Size);
  Font.Color := KeyValue.GetValue(Name + '.FontColor', Font.Color);
end;

procedure TAppConfig.SetFontInfo(Font: TFont; Name: String);
begin
  KeyValue.SetValue(Name + '.FontSize', Font.Size);
  KeyValue.SetValue(Name + '.FontColor', Font.Color);
end;

procedure TAppConfig.SaveConfig;
begin
  KeyValue.SaveFile;
end;


end.


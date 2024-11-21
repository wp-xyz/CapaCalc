unit ccManyCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  IniFiles, ccBaseFrame;

type

  { TManyCapFrame }

  TManyCapFrame = class(TBaseFrame)
    Bevel3: TBevel;
    BtnClear: TButton;
    EdC1: TEdit;
    EdC2: TEdit;
    EdC3: TEdit;
    EdC4: TEdit;
    EdC5: TEdit;
    EdC6: TEdit;
    EdC7: TEdit;
    EdC8: TEdit;
    LblC1: TLabel;
    LblC2: TLabel;
    LblC3: TLabel;
    LblC4: TLabel;
    LblC5: TLabel;
    LblC6: TLabel;
    LblC7: TLabel;
    LblC8: TLabel;
    LblCapa: TLabel;
    LblSameUnits: TLabel;
    Panel1: TPanel;
    TxtCapa: TEdit;
    UpDown1: TUpDown;
    procedure BtnClearClick(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private
    { private declarations }
  protected
    FDecimals: Integer;
    FEditArray: Array[1..8] of TEdit;
    procedure ClearResults; override;
    procedure SetEditLeft(AValue: Integer); override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Calculate; override;
    procedure ReadFromIni(ini: TCustomIniFile); override;
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean; override;
    procedure WriteToIni(ini: TCustomIniFile); override;
  end;

implementation

{$R *.lfm}

uses
  ccStrings;

constructor TManyCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDecimals := 2;
  FEditArray[1] := EdC1;
  FEditArray[2] := EdC2;
  FEditArray[3] := EdC3;
  FEditArray[4] := EdC4;
  FEditArray[5] := EdC5;
  FEditArray[6] := EdC6;
  FEditArray[7] := EdC7;
  FEditArray[8] := EdC8;
end;

procedure TManyCapFrame.BtnClearClick(Sender: TObject);
var
  i: Integer;
begin
  for i:= 1 to Length(FEditArray) do
    FEditArray[i].Clear;
  TxtCapa.Clear;
end;

procedure TManyCapFrame.Calculate;
var
  capa, c : double;
  i: Integer;
begin
  try
    capa := 0.0;
    for i:=1 to Length(FEditArray) do
      if (FEditArray[i].Text <> '') and TryStrToFloat(FEditArray[i].Text, c) then
        capa := capa + 1.0 / c;
    if capa <> 0.0 then
      TxtCapa.Text := Format('%.*f', [FDecimals, 1.0 / capa]) else
      TxtCapa.Clear;
  except
    TxtCapa.Clear;
  end;
end;

procedure TManyCapFrame.ClearResults;
begin
  TxtCapa.Clear;
end;

procedure TManyCapFrame.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    btNext: inc(FDecimals);
    btPrev: if FDecimals > 0 then dec(FDecimals);
  end;
  Calculate;
end;

procedure TManyCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  s: String;
  value: Extended;
  i: Integer;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  for i := 1 to Length(FEditArray) do
  begin
    s := ini.ReadString(FIniKey, 'C' + IntToStr(i), '');
    if (s <> '') and TryStrToFloat(s, value, fs) then
      FEditArray[i].Text := FloatToStr(value) else
      FEditArray[i].Clear;
  end;
  FDecimals := ini.ReadInteger(FIniKey, 'Decimals', FDecimals);
end;

procedure TManyCapFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  FEditLeft := AValue;
  EdC1.Left := FEditLeft;
  TxtCapa.Left := FEditLeft;
  TxtCapa.Width := EdC1.Width;
  Panel1.Height := TxtCapa.Top + TxtCapa.Height + TxtCapa.Top;
end;


function TManyCapFrame.ValidData(out AMsg: String;
  out AControl: TWinControl): Boolean;
var
  i: Integer;
  value: Extended;
begin
  Result := false;
  for i := 1 to Length(FEditArray) do
    if (FEditArray[i].Text <> '') then
    begin
      if not TryStrToFloat(FEditArray[i].Text, value) then
      begin
        AMsg := SNumberRequired;
        AControl := FEditArray[i];
        exit;
      end;
      if value <= 0 then begin
        AMsg := SPositiveNumberRequired;
        Acontrol := FEditArray[i];
        exit;
      end;
    end;
  Result := true;
end;

procedure TManyCapFrame.WriteToIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
  i: Integer;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini.EraseSection(FIniKey);

  for i:=1 to Length(FEditArray) do
    if (FEditArray[i].Text <> '') and TryStrToFloat(FEditArray[i].Text, value) then
      ini.WriteString(FIniKey, 'C' + IntToStr(i), FloatToStr(value, fs)) else
      ini.WriteString(FIniKey, 'C' + IntToStr(i), '');

  ini.WriteInteger(FIniKey, 'Decimals', FDecimals);
end;

end.


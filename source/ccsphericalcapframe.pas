unit ccSphericalCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, IniFiles,
  ccBaseFrame;

type

  { TSphericalCapFrame }

  TSphericalCapFrame = class(TBaseFrame)
    Bevel3: TBevel;
    CbAreaUnits: TComboBox;
    CbCapaUnits: TComboBox;
    CbRadiusAUnits: TComboBox;
    CbRadiusBUnits: TComboBox;
    EdRadiusA: TEdit;
    EdEps: TEdit;
    EdRadiusB: TEdit;
    LblArea: TLabel;
    LblCapa: TLabel;
    LblCapaPerArea: TLabel;
    LblRadiusA: TLabel;
    LblEps: TLabel;
    LblRadiusB: TLabel;
    Panel1: TPanel;
    TxtArea: TEdit;
    TxtCapa: TEdit;
    TxtCapaPerArea: TEdit;
    TxtCapaPerAreaUnits: TLabel;
  private
    { private declarations }
  protected
    procedure ClearResults; override;
    procedure SetEditLeft(AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Calculate; override;
    procedure ReadFromIni(ini: TCustomIniFile); override;
    function ValidData(out AMsg: String; out AControl: TWinControl): Boolean; override;
    procedure WriteToIni(ini: TCustomIniFile); override;
  end;

implementation

{$R *.lfm}

uses
  ccGlobal, ccStrings;

constructor TSphericalCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := 'Spherical';
end;

procedure TSphericalCapFrame.Calculate;
var
  ra, rb, eps : extended;
  A, Capa : double;
  fa, fc : double;
  capaFmt: String;
  capaUnits, areaUnits: string;
begin
  try
    if (EdRadiusA.Text = '') or not TryStrToFloat(EdRadiusA.Text, ra) then exit;
    if ra = 0 then exit;
    if (EdRadiusB.Text = '') or not TryStrToFloat(EdRadiusB.Text, rb) then exit;
    if ra >= rb then exit;
    if (EdEps.Text = '') or not TryStrToFloat(EdEps.Text, eps) then exit;
    if CbRadiusAUnits.ItemIndex = -1 then exit;
    if CbRadiusBUnits.ItemIndex = -1 then exit;
    if CbAreaUnits.ItemIndex = -1 then exit;
    if CbCapaUnits.ItemIndex = -1 then exit;

    ra := ra * Lenfactor[TLenUnits(CbRadiusAUnits.ItemIndex)];
    rb := rb * LenFactor[TLenUnits(CbRadiusBUnits.ItemIndex)];
    fa := AreaFactor[TAreaUnits(CbAreaUnits.ItemIndex)];
    fc := CapaFactor[TCapaUnits(CbCapaUnits.ItemIndex)];
    A := 1.0/6.0 * pi *(ra + rb)*(ra + rb)*(ra + rb);
    Capa := 4.0 * pi * eps0 * eps / (1.0/ra - 1.0/rb);

    if CbCapaUnits.Text='F' then
      capaFmt := CapaExpFormat else
      capaFmt := CapaStdFormat;
    capaUnits := CbCapaUnits.Items[CbCapaUnits.ItemIndex];
    areaUnits := CbAreaUnits.Items[CbAreaUnits.ItemIndex];

    // Area
    TxtArea.Text := FormatFloat(AreaStdFormat, A / fa);
    // Capacitance
    TxtCapa.Text := FormatFloat(capafmt, Capa / fc);
    // Capacitance per area
    TxtCapaPerArea.Text := FormatFloat(CapaPerAreaFormat, Capa / A * fa / fc);
    TxtCapaPerAreaUnits.Caption := Format('%s/%s', [capaUnits, areaUnits]);
  except
    ClearResults;
  end;
end;

procedure TSphericalCapFrame.ClearResults;
begin
  TxtCapa.Clear;
  TxtArea.Clear;
  TxtCapaPerArea.Clear;
end;

procedure TSphericalCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  s: String;
  value: Extended;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  s := ini.ReadString(FIniKey, 'Radius A', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdRadiusA.Text := FloatToStr(value) else
    EdRadiusA.Clear;

  s := ini.ReadString(FIniKey, 'Radius B', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdRadiusB.Text := FloatToStr(value) else
    EdRadiusB.Clear;

  s := ini.ReadString(FIniKey, 'eps', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdEps.Text := FloatToStr(value) else
    EdEps.Clear;

  s := ini.ReadString(FIniKey, 'Radius A units', '');
  if (s <> '') then
    CbRadiusAUnits.ItemIndex := CbRadiusAUnits.Items.IndexOf(s) else
    CbRadiusAUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Radius B units', '');
  if (s <> '') then
    CbRadiusBUnits.ItemIndex := CbRadiusBUnits.Items.IndexOf(s) else
    CbRadiusBUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Area units', '');
  if (s <> '') then
    CbAreaUnits.ItemIndex := CbAreaUnits.Items.IndexOf(s) else
    CbAreaUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Capa units', '');
  if (s <> '') then
    CbCapaUnits.ItemIndex := CbCapaUnits.Items.Indexof(s) else
    CbCapaUnits.ItemIndex := -1;

  Calculate;
end;

procedure TSphericalCapFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  inherited;

  EdRadiusA.Left := FEditLeft;
  TxtArea.Left := FEditLeft;
  TxtArea.Width := EdRadiusA.Width;

  Panel1.Height := TxtCapaPerArea.Top + TxtCapaPerArea.Height + TxtArea.Top;
  Width := CbRadiusAUnits.Left + CbRadiusAUnits.Width + 2*FControlDist;
end;

function TSphericalCapFrame.ValidData(out AMsg: String; out AControl: TWinControl
  ): Boolean;
begin
  Result := false;

  if not IsValidNumber(EdRadiusA, AMsg) then begin
    AControl := EdRadiusA;
    exit;
  end;

  if not IsValidNumber(EdRadiusB, AMsg) then begin
    AControl := EdRadiusB;
    exit;
  end;

  if not IsValidNumber(EdEps, AMsg) then begin
    AControl := EdEps;
    exit;
  end;

  if StrToFloat(EdRadiusA.Text) = StrToFloat(EdRadiusB.Text) then begin
    AControl := EdRadiusA;
    AMsg := SSphereRadiiMustNotBeEqual;
    exit;
  end;

  if StrToFloat(EdRadiusA.Text) > StrToFloat(EdRadiusB.Text) then begin
    AControl := EdRadiusB;
    AMsg := SInnerRadiusMustBeSmaller;
    exit;
  end;

  if not IsValidComboValue(CbRadiusAUnits, AMsg) then begin
    AControl := CbRadiusAUnits;
    exit;
  end;

  if not IsValidComboValue(CbRadiusBUnits, AMsg) then begin
    AControl := CbRadiusBUnits;
    exit;
  end;

  if not IsValidComboValue(CbAreaUnits, AMsg) then begin
    AControl := CbAreaUnits;
    exit;
  end;

  if not IsValidComboValue(CbCapaUnits, AMsg) then begin
    AControl := CbCapaUnits;
    exit;
  end;

  Result := true;
end;

procedure TSphericalCapFrame.WriteToIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini.EraseSection(FIniKey);

  if (EdRadiusA.Text <> '') and TryStrToFloat(EdRadiusA.Text, value) then
    ini.WriteString(FIniKey, 'Radius A', FloatToStr(value, fs));

  if CbRadiusAUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Radius A units', CbRadiusAUnits.Items[CbRadiusAUnits.ItemIndex]);

  if (EdRadiusB.Text <> '') and TryStrToFloat(EdRadiusB.Text, value) then
    ini.WriteString(FIniKey, 'Radius B', FloatToStr(value, fs));

  if CbRadiusBUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Radius B units', CbRadiusBUnits.Items[CbRadiusBUnits.ItemIndex]);

  if (EdEps.Text <> '') and TryStrToFloat(EdEps.Text, value) then
    ini.WriteString(FIniKey, 'eps', FloatToStr(value, fs));

  if CbAreaUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Area units', CbAreaUnits.Items[CbAreaUnits.ItemIndex]);

  if CbCapaUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Capa units', CbCapaUnits.Items[CbCapaUnits.ItemIndex]);
end;

end.


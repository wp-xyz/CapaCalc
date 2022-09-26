unit ccCylPlaneCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, IniFiles,
  ccBaseFrame;

type

  { TCylPlaneCapFrame }

  TCylPlaneCapFrame = class(TBaseFrame)
    Bevel3: TBevel;
    CbCapaUnits: TComboBox;
    CbDistUnits: TComboBox;
    CbRadiusUnits: TComboBox;
    CbLengthUnits: TComboBox;
    EdDist: TEdit;
    EdEps: TEdit;
    EdRadius: TEdit;
    EdLength: TEdit;
    LblCapa: TLabel;
    LblCapaPerLength: TLabel;
    LblDist: TLabel;
    LblEps: TLabel;
    LblRadius: TLabel;
    LblLength: TLabel;
    Panel1: TPanel;
    TxtCapa: TEdit;
    TxtCapaPerLength: TEdit;
    TxtCapaPerLengthUnits: TLabel;
  private
    { private declarations }
  protected
    procedure ClearResults; override;
    function DoCalc(d, R, L, eps: Extended): Extended; virtual;
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
  ccGlobal;

{ TCylPlaneCapFrame }

constructor TCylPlaneCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := 'Cylinder-plane';
end;

procedure TCylPlaneCapFrame.Calculate;
var
  R, d, L, capa, eps, fc, fL: extended;
  capaFmt, capaUnits, lengthUnits: String;
begin
  try
    // Validity check of parameters
    if (EdDist.Text = '') or not TryStrToFloat(EdDist.Text, d) then exit;
    if d = 0 then exit;
    if (EdRadius.Text = '') or not TryStrToFloat(EdRadius.Text, R) then exit;
    if R = 0 then exit;
    if (EdLength.Text = '') or not TryStrToFloat(EdLength.Text, L) then exit;
    if L = 0 then exit;
    if (EdEps.Text = '') or not TryStrToFloat(EdEps.Text, eps) then exit;
    if CbDistUnits.ItemIndex = -1 then exit;
    if CbRadiusUnits.ItemIndex = -1 then exit;
    if CbLengthUnits.ItemIndex = -1 then exit;
    if CbCapaUnits.ItemIndex = -1 then exit;

    // Convert parameter to SI units
    fL := LenFactor[TLenUnits(CbLengthUnits.ItemIndex)];
    fC := CapaFactor[TCapaUnits(CbCapaUnits.ItemIndex)];
    d := d * LenFactor[TLenUnits(CbDistUnits.ItemIndex)];
    R := R * LenFactor[TLenUnits(CbRadiusUnits.ItemIndex)];
    L := L * fL;

    // Calculate
    Capa := DoCalc(d, R, L, eps);

    // Display results
    if CbCapaUnits.Text = 'F' then
      capaFmt := CapaExpFormat else
      capaFmt := CapaStdFormat;
    capaUnits := CbCapaUnits.Items[CbCapaUnits.ItemIndex];
    lengthUnits := CbLengthUnits.Items[CbLengthUnits.ItemIndex];

    // Capacitance
    TxtCapa.Text := FormatFloat(capaFmt, capa / fc);
    // Capacitance per length
    TxtCapaPerLengthUnits.caption := Format('%s/%s', [capaUnits, lengthUnits]);
    TxtCapaPerLength.Text := FormatFloat(CapaPerLengthFormat, capa / L * fL / fC);
  except
    ClearResults;
  end;
end;

procedure TCylPlaneCapFrame.ClearResults;
begin
  TxtCapa.Clear;
  TxtCapaPerLength.Clear;
end;

function TCylPlaneCapFrame.DoCalc(d, R, L, eps: Extended): Extended;
begin
  Result := TwoPi * eps0 * eps * L / ln( (d + sqrt(d*d - R*R))/R);
end;

procedure TCylPlaneCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  s: String;
  value: Extended;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  s := ini.ReadString(FIniKey, 'Dist', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdDist.Text := FloatToStr(value) else
    EdDist.Clear;

  s := ini.ReadString(FIniKey, 'Length', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdLength.Text := FloatToStr(value) else
    EdLength.Clear;

  s := ini.ReadString(FIniKey, 'Radius', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdRadius.Text := FloatToStr(value) else
    EdRadius.Clear;

  s := ini.ReadString(FIniKey, 'eps', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdEps.Text := FloatToStr(value) else
    EdEps.Clear;

  s := ini.ReadString(FIniKey, 'Dist units', '');
  if (s <> '') then
    CbDistUnits.ItemIndex := CbDistUnits.Items.IndexOf(s) else
    CbDistUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Length units', '');
  if (s <> '') then
    CbLengthUnits.ItemIndex := CbLengthUnits.Items.IndexOf(s) else
    CbLengthUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Radius units', '');
  if (s <> '') then
    CbRadiusUnits.ItemIndex := CbRadiusUnits.Items.IndexOf(s) else
    CbRadiusUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Capa units', '');
  if (s <> '') then
    CbCapaUnits.ItemIndex := CbCapaUnits.Items.Indexof(s) else
    CbCapaUnits.ItemIndex := -1;

  Calculate;
end;

procedure TCylPlaneCapFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  inherited;

  EdDist.Left := FEditLeft;
  TxtCapa.Left := FEditLeft;
  TxtCapa.Width := EdDist.Width;

  Panel1.Height := TxtCapaPerLength.Top + TxtCapaPerLength.Height + TxtCapa.Top;

  Width := CbDistUnits.Left + CbDistUnits.Width + 2*FControlDist;
end;

function TCylPlaneCapFrame.ValidData(out AMsg: String; out AControl: TWinControl
  ): Boolean;
begin
  Result := false;

  if not IsValidPositive(EdDist, AMsg) then begin
    AControl := EdDist;
    exit;
  end;

  if not IsValidPositive(EdLength, AMsg) then begin
    AControl := EdLength;
    exit;
  end;

  if not IsValidPositive(EdRadius, AMsg) then begin
    AControl := EdRadius;
    exit;
  end;

  if not IsValidNumber(EdEps, AMsg) then begin
    AControl := EdEps;
    exit;
  end;

  if not IsValidComboValue(CbDistUnits, AMsg) then begin
    AControl := CbDistUnits;
    exit;
  end;

  if not IsValidComboValue(CbLengthUnits, AMsg) then begin
    AControl := CbLengthUnits;
    exit;
  end;

  if not IsValidComboValue(CbRadiusUnits, AMsg) then begin
    AControl := CbRadiusUnits;
    exit;
  end;

  if not IsValidComboValue(CbCapaUnits, AMsg) then begin
    AControl := CbCapaUnits;
    exit;
  end;

  Result := true;
end;

procedure TCylPlaneCapFrame.WriteToIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini.EraseSection(FIniKey);

  if (EdDist.Text <> '') and TryStrToFloat(EdDist.Text, value) then
    ini.WriteString(FIniKey, 'Dist', FloatToStr(value, fs));

  if CbDistUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Dist units', CbDistUnits.Items[CbDistUnits.ItemIndex]);

  if (EdLength.Text <> '') and TryStrToFloat(EdLength.Text, value) then
    ini.WriteString(FIniKey, 'Length', FloatToStr(value, fs));

  if CbLengthUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Length units', CbLengthUnits.Items[CbLengthUnits.ItemIndex]);

  if (EdRadius.Text <> '') and TryStrToFloat(EdRadius.Text, value) then
    ini.WriteString(FIniKEy, 'Radius', FloatToStr(value, fs));

  if CbRadiusUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Radius units', CbRadiusUnits.Items[CbRadiusUnits.ItemIndex]);

  if (EdEps.Text <> '') and TryStrToFloat(EdEps.Text, value) then
    ini.WriteString(FIniKey, 'eps', FloatToStr(value, fs));

  if CbCapaUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Capa units', CbCapaUnits.Items[CbCapaUnits.ItemIndex]);
end;

end.


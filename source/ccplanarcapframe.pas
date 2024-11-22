unit ccPlanarCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, IniFiles,
  ccBaseFrame;

type

  { TPlanarCapFrame }

  TPlanarCapFrame = class(TBaseFrame)
    Bevel3: TBevel;
    CbCapaUnits: TComboBox;
    CbDistUnits: TComboBox;
    CbAreaUnits: TComboBox;
    CbLengthUnits: TComboBox;
    CbWidthUnits: TComboBox;
    EdDist: TEdit;
    Panel1: TPanel;
    TxtCapaPerAreaUnits: TLabel;
    TxtCapaPerArea: TEdit;
    TxtCapaPerLength: TEdit;
    LblCapa: TLabel;
    TxtArea: TEdit;
    EdLength: TEdit;
    EdWidth: TEdit;
    EdEps: TEdit;
    LblDist: TLabel;
    LblArea: TLabel;
    LblLength: TLabel;
    LblWidth: TLabel;
    LblEps: TLabel;
    TxtCapa: TEdit;
    LblCapaPerArea: TLabel;
    LblCapaPerLength: TLabel;
    TxtCapaPerLengthUnits: TLabel;
  private
    { private declarations }
  protected
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
  ccGlobal;


{ TPlanarCapFrame }

constructor TPlanarcapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := 'Planar';
end;

procedure TPlanarCapFrame.Calculate;
var
  fL, fa, fc : double;
  d, W, L, A, eps, capa : extended;
  capaFmt: String;
  capaUnits: String;
  lengthUnits: String;
  areaUnits: String;
begin
  try
    if (EdDist.Text = '') or not TryStrToFloat(EdDist.Text, d) then exit;
    if d = 0 then exit;
    if (EdLength.Text = '') or not TryStrToFloat(EdLength.Text, L) then exit;
    if (EdWidth.Text = '') or not TryStrToFloat(EdWidth.Text, W) then exit;
    if (EdEps.Text = '') or not TryStrToFloat(EdEps.Text, eps) then exit;
    if CbDistUnits.ItemIndex = -1 then exit;
    if CbLengthUnits.ItemIndex = -1 then exit;
    if CbWidthUnits.ItemIndex = -1 then exit;
    if CbAreaUnits.ItemIndex = -1 then exit;
    if CbCapaUnits.ItemIndex = -1 then exit;

    fL := LenFactor[TLenUnits(CbLengthUnits.ItemIndex)];
    fa := AreaFactor[TAreaUnits(CbAreaUnits.ItemIndex)];
    fc := CapaFactor[TCapaUnits(CbCapaUnits.ItemIndex)];
    d := d * LenFactor[TLenUnits(CbDistUnits.ItemIndex)];
    W := W * LenFactor[TLenUnits(CbWidthUnits.ItemIndex)];
    L := L * fL;
    A := W * L;
    capa := eps0 * eps * A / d;

    if CbCapaUnits.Text = 'F' then
      capaFmt := CapaExpFormat else
      capaFmt := CapaStdFormat;

    capaUnits := CbCapaUnits.Items[CbCapaUnits.ItemIndex];
    lengthUnits := CbLengthUnits.Items[CbLengthUnits.ItemIndex];
    areaUnits := CbAreaUnits.Items[CbAreaUnits.ItemIndex];

    // Area
    TxtArea.Text := FormatFloat(AreaStdFormat, A / fa);
    // Capacitance
    TxtCapa.Text := FormatFloat(capaFmt, capa / fc);
    // Capacitance per area
    TxtCapaPerAreaUnits.Caption := Format('%s/%s', [capaUnits, areaUnits]);
    TxtCapaPerArea.Text := FormatFloat(CapaPerAreaFormat, capa / A * fa / fc);
    // Capacitance per length
    TxtCapaPerLengthUnits.caption := Format('%s/%s', [capaUnits, lengthUnits]);
    TxtCapaPerLength.Text := FormatFloat(CapaPerLengthFormat, capa / L * fL / fc);
  except
    ClearResults;
  end;
end;

procedure TPlanarCapFrame.ClearResults;
begin
  TxtArea.Clear;
  TxtCapa.Clear;
  TxtCapaPerArea.Clear;
  TxtCapaPerLength.Clear;
end;

procedure TPlanarCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  s: String;
  value: Extended;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  s := ini.ReadString(FIniKey, 'Dist', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdDist.Text := FloatToStr(value)
  else
    EdDist.Clear;

  s := ini.ReadString(FIniKey, 'Length', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdLength.Text := FloatToStr(value)
  else
    EdLength.Clear;

  s := ini.ReadString(FIniKey, 'Width', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdWidth.Text := FloatToStr(value)
  else
    EdWidth.Clear;

  s := ini.ReadString(FIniKey, 'eps', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdEps.Text := FloatToStr(value)
  else
    EdEps.Clear;

  s := ini.ReadString(FIniKey, 'Dist units', '');
  if (s <> '') then
    CbDistUnits.ItemIndex := CbDistUnits.Items.IndexOf(s)
  else
    CbDistUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Length units', '');
  if (s <> '') then
    CbLengthUnits.ItemIndex := CbLengthUnits.Items.IndexOf(s)
  else
    CbLengthUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Width units', '');
  if (s <> '') then
    CbWidthUnits.ItemIndex := CbWidthUnits.Items.IndexOf(s)
  else
    CbWidthUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Area units', '');
  if (s <> '') then
    CbAreaUnits.ItemIndex := CbAreaUnits.Items.IndexOf(s)
  else
    CbAreaUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Capa units', '');
  if (s <> '') then
    CbCapaUnits.ItemIndex := CbCapaUnits.Items.Indexof(s)
  else
    CbCapaUnits.ItemIndex := -1;
end;

procedure TPlanarCapFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  inherited;

  EdDist.Left := FEditLeft;
  TxtArea.BorderSpacing.Left := FEditLeft;
  Panel1.AutoSize := true;
//  TxtArea.Left := FEditLeft;

//  panel1.Height := TxtCapaPerLength.Top + TxtCapaPerLength.Height + TxtArea.Top;

  Width := CbDistUnits.Left + CbDistUnits.Width + 2*FControlDist;
end;

function TPlanarCapFrame.ValidData(out AMsg: String; out AControl: TWinControl
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

  if not IsValidPositive(EdWidth, AMsg) then begin
    AControl := EdWidth;
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

  if not IsValidComboValue(CbWidthUnits, AMsg) then begin
    AControl := CbWidthUnits;
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

procedure TPlanarCapFrame.WriteToIni(ini: TCustomIniFile);
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

  if (EdWidth.Text <> '') and TryStrToFloat(EdWidth.Text, value) then
    ini.WriteString(FIniKEy, 'Width', FloatToStr(value, fs));

  if CbWidthUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Width units', CbWidthUnits.Items[CbWidthUnits.ItemIndex]);

  if (EdEps.Text <> '') and TryStrToFloat(EdEps.Text, value) then
    ini.WriteString(FIniKey, 'eps', FloatToStr(value, fs));

  if CbAreaUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Area units', CbAreaUnits.Items[CbAreaUnits.ItemIndex]);

  if CbCapaUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Capa units', CbCapaUnits.Items[CbCapaUnits.ItemIndex]);
end;

end.


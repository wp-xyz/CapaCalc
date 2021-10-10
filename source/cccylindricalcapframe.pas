unit ccCylindricalCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, IniFiles,
  ccBaseFrame, ccSphericalCapFrame;

type

  { TCylindricalCapFrame }

  TCylindricalCapFrame = class(TSphericalCapFrame)
    CbLengthUnits: TComboBox;
    EdLength: TEdit;
    LblCapaPerLength: TLabel;
    LblLength: TLabel;
    TxtCapaPerLength: TEdit;
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

constructor TCylindricalCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := 'Cylindrical';
end;

procedure TCylindricalCapFrame.Calculate;
var
  ra, rb, eps, L, A, capa : extended;
  fa, fc, fL : double;
  capaFmt: String;
  capaUnits, areaUnits, lengthUnits: string;
begin
  try
    if (EdRadiusA.Text = '') or not TryStrToFloat(EdRadiusA.Text, ra) then exit;
    if ra <= 0 then exit;
    if (EdRadiusB.Text = '') or not TryStrToFloat(EdRadiusB.Text, rb) then exit;
    if (ra <= 0) or (rb <= 0) or (ra >= rb) then exit;
    if (EdLength.Text = '') or not TryStrToFloat(EdLength.Text, L) then exit;
    if (EdEps.Text = '') or not TryStrToFloat(EdEps.Text, eps) then exit;
    if CbRadiusAUnits.ItemIndex = -1 then exit;
    if CbRadiusBUnits.ItemIndex = -1 then exit;
    if CbLengthUnits.ItemIndex = -1 then exit;
    if CbAreaUnits.ItemIndex = -1 then exit;
    if CbCapaUnits.ItemIndex = -1 then exit;

    ra := ra * Lenfactor[TLenUnits(CbRadiusAUnits.ItemIndex)];
    rb := rb * LenFactor[TLenUnits(CbRadiusBUnits.ItemIndex)];
    L := L * Lenfactor[TLenUnits(CbLengthUnits.ItemIndex)];
    fL := LenFactor[TLenUnits(CbLengthUnits.ItemIndex)];
    fa := AreaFactor[TAreaUnits(CbAreaUnits.ItemIndex)];
    fc := CapaFactor[TCapaUnits(CbCapaUnits.ItemIndex)];

    A := pi * (ra + rb) * L;
    Capa := TwoPi * eps0 * eps * L / ln(rb / ra);

    if CbCapaUnits.Text='F' then
      capaFmt := CapaExpFormat else
      capaFmt := CapaStdFormat;
    capaUnits := CbCapaUnits.Items[CbCapaUnits.ItemIndex];
    areaUnits := CbAreaUnits.Items[CbAreaUnits.ItemIndex];
    lengthUnits := CbLengthUnits.Items[CbLengthUnits.ItemIndex];

    // Area
    TxtArea.Text := FormatFloat(AreaStdFormat, A / fa);
    // Capacitance
    TxtCapa.Text := FormatFloat(capafmt, capa / fc);
    // Capacitance per area
    TxtCapaPerArea.Text := FormatFloat(CapaPerAreaFormat, (capa / A) * (fa / fc));
    TxtCapaPerAreaUnits.Caption := Format('%s/%s', [capaUnits, areaUnits]);
    // Capacitance per length
    TxtCapaPerLengthUnits.caption := Format('%s/%s', [capaUnits, lengthUnits]);
    TxtCapaPerLength.Text := FormatFloat(CapaPerLengthFormat, (capa / L) * (fL / fc));
  except
    ClearResults;
  end;
end;

procedure TCylindricalCapFrame.ClearResults;
begin
  inherited;
  TxtCapaPerLength.Clear;
end;

procedure TCylindricalCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  s: String;
  value: Extended;
begin
  inherited ReadFromIni(ini);

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  s := ini.ReadString(FIniKey, 'Length', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdLength.Text := FloatToStr(value) else
    EdLength.Clear;

  s := ini.ReadString(FIniKey, 'Length units', '');
  if (s <> '') then
    CbLengthUnits.ItemIndex := CbLengthUnits.Items.IndexOf(s) else
    CbLengthUnits.ItemIndex := -1;

  Calculate;
end;

procedure TcylindricalCapFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  inherited;
  Panel1. Height := TxtCapaPerLength.Top + TxtCapaPerLength.Height + TxtArea.Top;
end;

function TCylindricalCapFrame.ValidData(out AMsg: String; out AControl: TWinControl
  ): Boolean;
begin
  Result := inherited ValidData(AMsg, AControl);
  if not Result then
    exit;

  if not IsValidNumber(EdLength, AMsg) then begin
    AControl := EdLength;
    exit;
  end;

  Result := true;
end;

procedure TCylindricalCapFrame.WriteToIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
begin
  inherited WriteToIni(ini);

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  if (EdLength.Text <> '') and TryStrToFloat(EdLength.Text, value) then
    ini.WriteString(FIniKey, 'Length', FloatToStr(value, fs));

  if CbLengthUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Length units', CbLengthUnits.Items[CbLengthUnits.ItemIndex]);
end;

end.


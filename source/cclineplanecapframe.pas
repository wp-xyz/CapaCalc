unit ccLinePlaneCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, IniFiles,
  ccPlanarCapFrame;

type

  { TLinePlaneCapFrame }

  TLinePlaneCapFrame = class(TPlanarCapFrame)
    CbHeightUnits: TComboBox;
    EdHeight: TEdit;
    LblHeight: TLabel;
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
  ccGlobal, ccStrings;

{ TLinePlaneCapFrame }

constructor TLinePlaneCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := 'Line-plane';
end;

procedure TLinePlaneCapFrame.Calculate;
var
  W, H, d, L, eps, capa, Wd, Hd: extended;
  fL, fc: Double;
  capaUnits, lengthUnits: String;
  capaFmt: String;
begin
  try
    if (EdDist.Text = '') or not TryStrToFloat(EdDist.Text, d) then exit;
    if d = 0 then exit;
    if (EdLength.Text = '') or not TryStrToFloat(EdLength.Text, L) then exit;
    if L = 0 then exit;
    if (EdWidth.Text = '') or not TryStrToFloat(EdWidth.Text, W) then exit;
    if W = 0 then exit;
    if (EdHeight.Text = '') or not TryStrToFloat(EdHeight.Text, H) then exit;
    if H = 0 then exit;
    if (EdEps.Text = '') or not TryStrToFloat(EdEps.Text, eps) then exit;
    if CbDistUnits.ItemIndex = -1 then exit;
    if CbLengthUnits.ItemIndex = -1 then exit;
    if CbWidthUnits.ItemIndex = -1 then exit;
    if CbHeightUnits.ItemIndex = -1 then exit;
    if CbCapaUnits.ItemIndex = -1 then exit;

    fL := LenFactor[TLenUnits(CbLengthUnits.ItemIndex)];
    fc := CapaFactor[TCapaUnits(CbCapaUnits.ItemIndex)];
    d := d * LenFactor[TLenUnits(CbDistUnits.ItemIndex)];
    W := W * LenFactor[TLenUnits(CbWidthUnits.ItemIndex)];
    H := H * Lenfactor[TLenUnits(CbHeightUnits.ItemIndex)];
    L := L * fL;

    capa := eps0 * eps * L * (W/d + 0.77 + 1.06*sqrt(sqrt(W/d)) + 1.06*sqrt(H/d));

    if CbCapaUnits.Text = 'F' then
      capaFmt := CapaExpFormat else
      capaFmt := CapaStdFormat;
    capaUnits := CbCapaUnits.Items[CbCapaUnits.ItemIndex];
    lengthUnits := CbLengthUnits.Items[CbLengthUnits.ItemIndex];

    // Capacitance
    TxtCapa.Text := FormatFloat(capaFmt, capa / fc);
    // Capacitance per length
    TxtCapaPerLengthUnits.caption := Format('%s/%s', [capaUnits, lengthUnits]);
    TxtCapaPerLength.Text := FormatFloat(CapaPerLengthFormat, capa / L * fL / fc);

    // Notification of accuracy
    if Assigned(OnCalcComplete) then begin
      Wd := W / d;
      Hd := H / d;
      if (Wd > 0.3) and (Hd < 10) then begin
        if (Wd > 1.0) and (Hd > 0.1) and (Hd <=4) then
          OnCalcComplete(self, SAccuracy2Percent, true)
        else
          OnCalcComplete(self, SAccuracy6Percent, true);
      end else
        OnCalcComplete(self, SInaccurate, false);
    end;
  except
    ClearResults;
  end;
end;

procedure TLinePlaneCapFrame.ClearResults;
begin
  TxtCapa.Clear;
  TxtCapaPerLength.Clear;
end;

procedure TLinePlaneCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  s: String;
  value: Extended;
begin
  inherited ReadFromIni(ini);

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  s := ini.ReadString(FIniKey, 'Height', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdHeight.Text := FloatToStr(value) else
    EdHeight.Clear;

  s := ini.ReadString(FIniKey, 'Height units', '');
  if (s <> '') then
    CbHeightUnits.ItemIndex := CbHeightUnits.Items.IndexOf(s) else
    CbHeightUnits.ItemIndex := -1;

  // This control is inherited but not used. It needs a good default values.
  // Otherwise it would not pass the validity check.
  CbAreaUnits.ItemIndex := 0;
end;

procedure TLinePlaneCapFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  FEditLeft := AValue;
  EdDist.Left := FEditLeft;
  TxtCapa.BorderSpacing.Left := FEditLeft;
  TxtCapa.Width := EdDist.Width;
  Panel1.AutoSize := true;

  Width := CbDistUnits.Left + CbDistUnits.Width + 2*FControlDist + ImagePanel.Width;
end;

function TLinePlaneCapFrame.ValidData(out AMsg: String; out AControl: TWinControl
  ): Boolean;
begin
  Result := inherited ValidData(AMsg, AControl);
  if not Result then
    exit;

  if not IsValidPositive(EdHeight, AMsg) then begin
    AControl := EdHeight;
    exit;
  end;

  Result := true;
end;

procedure TLinePlaneCapFrame.WriteToIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
begin
  inherited WriteToIni(ini);

  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  if (EdHeight.Text <> '') and TryStrToFloat(EdHeight.Text, value) then
    ini.WriteString(FIniKEy, 'Height', FloatToStr(value, fs));

  if CbHeightUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Height units', CbHeightUnits.Items[CbHeightUnits.ItemIndex]);
end;


end.


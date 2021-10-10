unit ccPNJunctionCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, ExtCtrls, StdCtrls, Spin, IniFiles,
  ccBaseFrame;

type

  { TPNJunctionCapFrame }

  TPNJunctionCapFrame = class(TBaseFrame)
    Bevel2: TBevel;
    Bevel3: TBevel;
    CbEMaxUnits: TComboBox;
    CbMaterial: TComboBox;
    CbJctType: TComboBox;
    CbDepletionWidthUnits: TComboBox;
    CbNdUnits: TComboBox;
    CbDeltaConcUnits: TComboBox;
    EdNa: TEdit;
    CbTempUnits: TComboBox;
    LblBuiltinVoltag1: TLabel;
    LblCapaPerAreaUnits: TLabel;
    Panel1: TPanel;
    TxtCapaPerArea: TEdit;
    TxtDepletionWidth: TEdit;
    EdNd: TEdit;
    EdDeltaConc: TEdit;
    EdTemp: TEdit;
    CbNaUnits: TComboBox;
    EdVoltage: TFloatSpinEdit;
    LblEMax: TLabel;
    LblBuiltinVoltag: TLabel;
    LblNa: TLabel;
    LblDepletionWidth: TLabel;
    LblNd: TLabel;
    LblMaterial: TLabel;
    LblJctType: TLabel;
    LblDeltaConc: TLabel;
    LblDeltaConcPerMicron: TLabel;
    LblTemp: TLabel;
    LblVoltage: TLabel;
    LblVoltageUnits: TLabel;
    LblBuiltinVoltageUnits: TLabel;
    TxtEMax: TEdit;
    TxtBuiltinVoltage: TEdit;
    procedure CbJctTypeChange(Sender: TObject);
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
  Math, ccGlobal, ccStrings;

type
  TMathFunc = function(x: extended; const params: array of extended) : extended;

{ Finds the zero of the function f in the interval [x1, x2] with precision tol.
  The result is false if an intersection is not found.

  Algorithmus: Van Wijngaarden-Dekker-Brent-Methode (Press, Num Recipes, p 283) }
function FindRoot(f: TMathFunc; x1,x2,tol: extended;
  const Params:array of extended; var x0: extended): boolean;
const
  ItMax = 100;
  FloatEps = 3.4E-9;
var
  a, b, c, d, e   : extended;
  min1, min2, min : extended;
  fa, fb, fc      : extended;
  p, q, r         : extended;
  s, tol1, xm     : extended;
  iter            : integer;
begin
  result := false;
  try
    a  := x1;
    b  := x2;
    fa := f(x1, params);
    if IsNaN(fa) then exit;
    fb := f(x2, params);
    if IsNaN(fb) then exit;
    if fa*fb > 0.0 then exit;

    fc := fb;
    for iter := 1 to ItMax do begin
      if fb*fc > 0.0 then begin
        c  := a;
        fc := fa;
        d  := b - a;
        e  := d;
      end;
      if abs(fc) < abs(fb) then begin
        a  := b;
        b  := c;
        c  := a;
        fa := fb;
        fb := fc;
        fc := fa;
      end;
      tol1 := 2.0 * FloatEps * abs(b) + 0.5 * tol;
      xm   := 0.5 * (c - b);
      if (abs(xm) <= tol1) or (fb = 0.0) then begin
        x0 := b;
        result := true;
        exit;
      end;
      if (abs(xm) <= tol1) and (abs(fa) > abs(fb)) then begin
        s  := fb / fa;
        if a = c then begin
          p := 2.0 * xm * s;
          q := 1.0 - s;
        end else begin
          q := fa / fc;
          r := fb / fc;
          p := s * (2 * xm * q * (q-r) - (b-a) * (r-1));
          q := (q-1) * (r-1) * (s-1);
        end;
        if (p > 0.0) then q := -q;
        p := abs(p);
        min1 := 3.0 * xm * q - abs(tol1 * q);
        min2 := abs(e * q);
        if min1 < min2
          then min := min1
          else min := min2;
        if 2.0 * p < min then begin
          e := d;
          d := p / q;
        end else begin
          d := xm;
          e := d;
        end;
      end else begin
        d := xm;
        e := d;
      end;
      a  := b;
      fa := fb;
      if abs(d) > tol1 then
        b := b + d
      else begin
        if xm >= 0 then b := b + abs(tol1) else b := b - abs(tol1);
      end;
      fb := f(b, params);
      if IsNaN(fb) then exit;
    end;
    x0 := b;
    result := true;
  except
  end;
end;

{ Helper function to calculate the built-in voltage V0 of a p/n junction with
  linear concentration gradient.
  Use FindRoot to determine ther zero of this function. }
function Calc_BuiltinVoltage(V0: extended; const params:array of extended): extended;
var
  a, eps, V, T, ni : extended;
  arg : extended;
  x: extended;
begin
  a := params[0];
  eps := params[1];
  V := params[2];
  T := params[3];
  ni := params[4];
  x := 12.0 * eps * (V0 - V) * a*a / q;
  if x < 0 then begin
    Result := NaN;
    exit;
  end;
  arg := power(x, 1.0/3) / (2*ni);
  result := 2*kB*T/q * ln(arg) - V0;
end;

{ Calculates the temperature dependence of intrinsic density, in m-3. }
function Calc_ni(AMaterial: TMaterial; T: double) : double;
begin
  case AMaterial of
    mSi : result := 5.29E19 * power(T/300, 2.54) * exp(-6726/T);  // in 1/cm³
          // Ref. www.pveducation.org/pvcdrom/pn-junction/intrinsic-carrier-concentration
    {
    mSi : result := 9.38E19 * sqr(T/300) * exp(-6884/T);
          // Ref. http://www.udel.edu/igert/pvcdrom/SEMICON/NI.HTM
          // 300 K --> 1.01E10
          }
    else
      if T = 300 then
        result := ni[AMaterial]
      else
        raise Exception.Create(STemperatureDependenceNotSupported);
  end;
  result := result / VolumeFactor[vu_cm3]; // 1/cm³ --> 1/m3
end;

function NumToStr(AValue: Extended): String;
begin
  if AValue < 1E-4 then
    Result := FormatFloat(CapaExpFormat, AValue)
  else if AValue > 1E4 then
    Result := FormatFloat(CapaExpFormat, AValue)
  else
    Result := FormatFloat(CapaStdFormat, AValue);
end;


{ TPNJunctionCapFrame }

constructor TPNJunctionCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := 'p/n junction';

  EdDeltaConc.Top := EdNa.Top;
  CbDeltaConcUnits.Top := CbNaUnits.Top;
  LblDeltaConc.Top := LblNa.Top;
  LblDeltaConcPerMicron.Top := LblNa.Top;
end;

procedure TPNJunctionCapFrame.Calculate;
var
  kToverQ : extended;      // kT/q
  T : extended;            // temperature
  ND, NA, a : extended;
  material : TMaterial;
  V0: extended;           // Built-in voltage
  W: extended;            // Junction width
  W0: extended;           // junction width at V=0
  V: extended;            // applied voltage
  Cj0: extended;          // junction capacitance at V=0
  Cj: extended;           // junction capacitance
  emax: extended;         // max field
  deltaConc: extended;    // concentration gradient
  fv, fa, fL, fc: extended; // unit conversion factors for volume, area, length and capa
  fE, fCA, fCD, fdN: extended;  // ... max field, acceptor, donator concentration
  tmp1, tmp2 : extended;
  params : array of extended;
begin
  FErrMsg := '';
  try
    if (CbMaterial.ItemIndex = -1) then exit;
    if (EdTemp.Text = '') or not TryStrToFloat(EdTemp.Text, T) then exit;
    if T <= 0 then exit;
    if (CbJctType.ItemIndex = -1) then exit;
    if (EdVoltage.Text = '') then exit;
    if (EdNa.Text = '') or not TryStrToFloat(EdNa.Text, NA) then exit;
    if Na <= 0 then exit;
    if (EdNd.Text = '') or not TryStrToFloat(EdNd.Text, ND) then exit;
    if Nd <= 0 then exit;
    if (CbNaUnits.ItemIndex = -1) then exit;
    if (CbNdUnits.ItemIndex = -1) then exit;
    if CbJctType.ItemIndex = 1 then begin // graded junction
      if (EdDeltaConc.Text = '') or not TryStrToFloat(EdDeltaConc.Text, deltaConc) then exit;
      if (CbDeltaConcUnits.ItemIndex = -1) then exit;
    end;
    if CbEMaxUnits.ItemIndex = -1 then exit;
    if CbDepletionWidthUnits.ItemIndex = -1 then exit;

    fa := AreaFactor[au_cm2];
    fc := CapaFactor[cu_nF];
    fL := LenFactor[TLenUnits(CbDepletionWidthUnits.ItemIndex)];
    fE := EFieldFactor[TEFieldUnits(CbEMaxUnits.ItemIndex)];

    material := TMaterial(CbMaterial.ItemIndex);
    if CbTempUnits.ItemIndex = ord(tuC) then
      T := T + 273;
    kToverQ := kB * T / q;
    V := EdVoltage.Value;

    case CbJctType.ItemIndex of
      0: // *** abrupt junction ***
        begin
          fCA := 1.0 / VolumeFactor[TVolumeUnits(CbNaUnits.ItemIndex)];
          fCD := 1.0 / VolumeFactor[TVolumeUnits(CbNdUnits.ItemIndex)];
          // Dopant concentrations (1/m3)
          NA := NA * fCA;
          ND := ND * fCD;
          // built-in voltage (V)
          V0 := kToverQ * ln( NA * ND / sqr(Calc_ni(material, T)) );
          if (V > V0) then begin
            FErrMsg := Format(SVoltageMustBeSmallerThanBuiltIn, [V0]);
            ClearResults;
            exit;
          end;
          // jct width (m)
          W := sqrt (2 * MaterialEps[material]/q * (NA + ND)/(NA * ND) * (V0 - V) );
         // jct capa per area (F/m2)
         Cj := MaterialEps[material] / W;
         // max field (V/m)
         emax := 2.0 * (V0 - V) / W;
       end;

      1: // *** linearly graded junction ***
        begin
          // Concentration gradient dN/dx (1/m4), 1E6 because of dx = 1 micron
          fdN := 1.0 / VolumeFactor[TVolumeUnits(CbDeltaConcUnits.ItemIndex)];
          a := deltaConc * fdN * 1E6;
          // Find jct width and built-in voltage. Solve nonlinear equation system
          // w = power(12 eps (V0 - V) / ( q a), 1/3)   ( units: m)
          // V0 = 2 kT/q ln ( aw / (2 ni) )             ( units: V)
          SetLength(params, 5);
          params[0] := a;
          params[1] := MaterialEps[material];
          params[2] := V;
          params[3] := T;
          params[4] := Calc_ni(material, T);
          if FindRoot(@Calc_BuiltinVoltage, 1E-1, 2.0, 1E-3, params, V0) then begin
            if (V > V0) then begin
              FErrMsg := Format(SVoltageMustBeSmallerThanBuiltIn, [V0]);
              ClearResults;
              exit;
            end;
            W := power(12 * MaterialEps[material] * (V0 - V) / (q * a), 1/3);
          end else begin
            // use approximate solution
            tmp1 := sqr(a) * MaterialEps[material] * kToverQ;
            tmp2 := 8 * q * power(Calc_ni(material, T), 3);
            V0 := 2/3 * kToverQ * ln( tmp1/tmp2);
            V0 := V0 + 0.075;  // compensate for numerical approximation used in above eqns.
            if (V > V0) then begin
              ClearResults;
              FErrMsg := Format(SVoltageMustBeSmallerThanBuiltIn, [V0]);
              exit;
            end;
            //  raise Exception.CreateFmt(SVoltageMustBeSmallerThanBuiltIn, [V0]);
            W := power( 12 * MaterialEps[material] * (V0 - V) / (q*a), 1/3);
          end;
          // jct capa per area (F/m2)
          Cj := MaterialEps[material] / W;
          // max field (V/m)
          emax := q * a * sqr(W) / (8 * MaterialEps[material]);
        end;
    end;
    TxtEMax.Text := NumToStr(emax / fE);
    TxtBuiltInVoltage.Text := NumToStr(V0);
    TxtDepletionWidth.Text := NumToStr(W / fL);
    TxtCapaPerArea.Text := NumToStr(Cj / (fc/fa));

  except
    on E:Exception do begin
      ClearResults;
      FErrMsg := E.Message;
    end;
  end;
end;

procedure TPNJunctionCapFrame.CbJctTypeChange(Sender: TObject);
begin
  DataChanged(Sender);

  LblNa.Visible := CbJctType.ItemIndex = 0;
  EdNa.Visible := CbJctType.ItemIndex = 0;
  CbNaUnits.Visible := CbJctType.ItemIndex = 0;

  LblNd.Visible := CbJctType.ItemIndex = 0;
  EdNd.Visible := CbJctType.ItemIndex = 0;
  CbNdUnits.Visible := CbJctType.ItemIndex = 0;

  LblDeltaConc.Visible := CbJctType.ItemIndex = 1;
  EdDeltaConc.Visible := CbJctType.ItemIndex = 1;
  CbDeltaConcUnits.Visible := CbJctType.ItemIndex = 1;
  LblDeltaConcPerMicron.Visible := CbJctType.ItemIndex = 1;
end;

procedure TPNJunctionCapFrame.ClearResults;
begin
  TxtEMax.Clear;
  TxtBuiltinVoltage.Clear;
  TxtDepletionWidth.Clear;
  TxtCapaPerArea.Clear;
end;

procedure TPNJunctionCapFrame.ReadFromIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
  s: String;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  s := ini.ReadString(FIniKey, 'Material', '');
  if (s <> '') then
    CbMaterial.ItemIndex := CbMaterial.Items.IndexOf(s) else
    CbMaterial.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Temperature', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdTemp.Text := FloatToStr(value) else
    EdTemp.Clear;

  s := ini.ReadString(FIniKey, 'Temperature units', '');
  if (s <> '') then
    CbTempUnits.ItemIndex := CbTempUnits.Items.IndexOf(s) else
    CbTempUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Junction type', '');
  if (s <> '') then
    CbJctType.ItemIndex := CbJctType.Items.IndexOf(s) else
    CbJcttype.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Voltage', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdVoltage.Value := value else
    EdVoltage.Clear;

  s := ini.ReadString(FIniKey, 'Na', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdNa.Text := FormatFloat(ConcFormat, value) else
    EdNa.Clear;

  s := ini.Readstring(FIniKey, 'Na units', '');
  if s <> '' then
    CbNaUnits.ItemIndex := CbNaUnits.Items.IndexOf(s) else
    CbNaUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Nd', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdNd.Text := FormatFloat(ConcFormat, value) else
    EdNd.Clear;

  s := ini.Readstring(FIniKey, 'Nd units', '');
  if s <> '' then
    CbNdUnits.ItemIndex := CbNdUnits.Items.IndexOf(s) else
    CbNdUnits.ItemIndex := -1;

  s := ini.ReadString(FIniKey, 'Concentration change', '');
  if (s <> '') and TryStrToFloat(s, value, fs) then
    EdDeltaConc.Text := FormatFloat(ConcFormat, value) else
    EdDeltaConc.Clear;

  s := ini.Readstring(FIniKey, 'Concentration change units', '');
  if s <> '' then
    CbDeltaConcUnits.ItemIndex := CbDeltaConcUnits.Items.IndexOf(s) else
    CbDeltaConcUnits.ItemIndex := -1;

  s := ini.Readstring(FIniKey, 'Field units', '');
  if s <> '' then
    CbEMaxUnits.ItemIndex := CbEMaxUnits.Items.IndexOf(s) else
    CbEMaxUnits.ItemIndex := -1;

  s := ini.Readstring(FIniKey, 'Field units', '');
  if s <> '' then
    CbEMaxUnits.ItemIndex := CbEMaxUnits.Items.IndexOf(s) else
    CbEMaxUnits.ItemIndex := -1;

  s := ini.Readstring(FIniKey, 'Depletion width units', '');
  if s <> '' then
    CbDepletionWidthUnits.ItemIndex := CbDepletionWidthUnits.Items.IndexOf(s) else
    CbDepletionWidthUnits.ItemIndex := -1;

  CbJctTypeChange(nil);

  Calculate;
end;

procedure TPNJunctionCapFrame.SetEditLeft(AValue: Integer);
begin
  TxtEMax.Left := CbMaterial.Left;
  Panel1.Height := TxtCapaPerArea.Top + TxtCapaPerArea.Height + TxtEMax.Top;
  Width := CbTempUnits.Left + CbTempUnits.Width + 30;
end;

function TPNJunctionCapFrame.ValidData(out AMsg: String;
  out AControl: TWinControl): Boolean;
begin
  Result := false;

  if not IsValidComboValue(CbMaterial, AMsg) then begin
    AControl := CbMaterial;
    exit;
  end;

  if not IsValidNumber(EdTemp, AMsg) then begin
    AControl := EdTemp;
    exit;
  end;

  if not IsValidComboValue(CbTempUnits, AMsg) then begin
    AControl := CbTempUnits;
    exit;
  end;

  if (StrToFloat(EdTemp.Text) <= 0) and (CbTempUnits.ItemIndex = ord(tuK))
  then begin
    AControl := EdTemp;
    AMsg := STemperaturePositive;
    exit;
  end;

  if not IsValidComboValue(CbJctType, AMsg) then begin
    AControl := CbJctType;
    exit;
  end;

  if not IsValidPositive(EdNa, AMsg) then begin
    AControl := EdNa;
    exit;
  end;

  if not IsValidPositive(EdNd, AMsg) then begin
    AControl := EdNd;
    exit;
  end;

  if CbJctType.ItemIndex = 1 then begin
    if not IsValidNumber(EdDeltaConc, AMsg) then begin
      AControl := EdDeltaConc;
      exit;
    end;
    if not IsValidComboValue(CbDeltaConcUnits, AMsg) then begin
      AControl := CbDeltaConcUnits;
      exit;
    end;
  end;

  if not IsvalidComboValue(CbEMaxUnits, AMsg) then begin
    AControl := CbEMaxUnits;
    exit;
  end;

  if not IsValidComboValue(CbDepletionWidthUnits, AMsg) then begin
    AControl := CbDepletionWidthUnits;
    exit;
  end;

  Result := True;
end;


procedure TPNJunctionCapFrame.WriteToIni(ini: TCustomIniFile);
var
  fs: TFormatSettings;
  value: Extended;
begin
  fs := DefaultFormatSettings;
  fs.DecimalSeparator := '.';

  ini.EraseSection(FIniKey);

  if (CbMaterial.ItemIndex >= 0) then
    ini.WriteString(FIniKey, 'Material', CbMaterial.Items[CbMaterial.ItemIndex]);

  if (EdTemp.Text <> '') and TryStrToFloat(EdTemp.Text, value) then
    ini.WriteString(FIniKey, 'Temperature', FloatToStr(value, fs));

  if CbTempUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Temperature units', CbTempUnits.Items[CbTempUnits.ItemIndex]);

  if CbJctType.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Junction type', CbJctType.Items[CbJctType.ItemIndex]);

  if (EdVoltage.Text <> '') then
    ini.WriteString(FIniKey, 'Voltage', FloatToStr(EdVoltage.Value, fs));

  if (EdNa.Text <> '') and TryStrToFloat(EdNa.Text, value) then
    ini.WriteString(FIniKey, 'Na', FloatToStr(value, fs));

  if CbNaUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Na units', CbNaUnits.Items[CbNaUnits.ItemIndex]);

  if (EdNd.Text <> '') and TryStrToFloat(EdNd.Text, value) then
    ini.WriteString(FIniKey, 'Nd', FloatToStr(value, fs));

  if CbNdUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Nd units', CbNdUnits.Items[CbNdUnits.ItemIndex]);

  if (EdDeltaConc.Text <> '') and TryStrToFloat(EdDeltaConc.Text, value) then
    ini.WriteString(FIniKey, 'Concentration change', FloatToStr(value, fs));

  if CbDeltaConcUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Concentration change units', CbDeltaConcUnits.Items[CbDeltaConcUnits.ItemIndex]);

  if CbEMaxUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Field units', CbEMaxUnits.Items[CbEMaxUnits.ItemIndex]);

  if CbDepletionWidthUnits.ItemIndex >= 0 then
    ini.WriteString(FIniKey, 'Depletion width units', CbDepletionWidthUnits.Items[CbDepletionWidthUnits.ItemIndex]);
end;

end.


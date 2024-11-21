unit ccBaseFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IniFiles,
  Forms, Controls, ExtCtrls, StdCtrls, Dialogs;

type

  TCalcCompleteEvent = procedure (Sender: TObject; AMsg: String;
    IsOK: Boolean) of object;

  { TBaseFrame }

  TBaseFrame = class(TFrame)
    Bevel1: TBevel;
    ImageBevel: TBevel;
    GeometryImage: TImage;
    EquationImage: TImage;
    ImagePanel: TPanel;
    Panel2: TPanel;
    procedure DataChanged(Sender: TObject);
    procedure UnitsChanged(Sender: TObject);
  private
    { private declarations }
    FOnCalcComplete: TCalcCompleteEvent;
  protected
    FIniKey: String;
    FEditLeft: Integer;
    FControlDist: Integer;
    FErrMsg: string;
    procedure ClearResults; virtual;
    function IsValidComboValue(AControl: TWinControl; out AMsg: String): Boolean;
    function IsValidNumber(AControl: TWinControl; out AMsg: String): Boolean;
    function IsValidPositive(AControl: TWinControl; out AMsg: String): Boolean;
    procedure SetEditLeft(AValue: Integer); virtual;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Calculate; virtual;
    procedure CheckAndCalculate;
    function ValidData(out {%H-}AMsg: String; out {%H-}AControl: TWinControl): Boolean; virtual;
    procedure ReadFromIni({%H-}ini: TCustomIniFile); virtual;
    procedure WriteToIni({%H-}ini: TCustomIniFile); virtual;
    property ControlDist: integer read FControlDist;
    property EditLeft: Integer read FEditLeft write SetEditLeft;
    property ErrMsg: String read FErrMsg;
    property OnCalcComplete: TCalcCompleteEvent read FOnCalcComplete write FOnCalcComplete;
  end;

  TBaseFrameClass = class of TBaseFrame;


implementation

{$R *.lfm}

uses
  ccStrings;

constructor TBaseFrame.Create(AOwner: TComponent);
begin
  inherited;
  FControlDist := 12;
end;

procedure TBaseFrame.Calculate;
begin
end;

procedure TBaseFrame.CheckAndCalculate;
var
  C: TWinControl = nil;
begin
  FErrMsg := '';
  if ValidData(FErrMsg, C) then
    Calculate;
  if FErrMsg <> '' then
  begin
    if Assigned(C) and C.CanFocus then C.SetFocus;
    MessageDlg(FErrMsg, mtError, [mbOK], 0);
  end;
end;

procedure TBaseFrame.ClearResults;
begin
end;

procedure TBaseFrame.DataChanged(Sender: TObject);
begin
  ClearResults;
end;

function TBaseFrame.IsValidNumber(AControl: TWinControl; out AMsg: String): Boolean;
var
  value: Extended;
begin
  Result := false;
  if (AControl is TEdit) then begin
    if (AControl as TEdit).Text = '' then begin
      AMsg := SInputRequired;
      exit;
    end;
    if not TryStrToFloat((AControl as TEdit).Text, value) then begin
      AMsg := SNumberRequired;
      exit;
    end;
  end;
  Result := true;
end;

function TBaseFrame.IsValidPositive(AControl: TWinControl; out AMsg: String): Boolean;
var
  value: Double;
begin
  Result := false;
  if (AControl is TEdit) then
  begin
    if (AControl as TEdit).Text = '' then begin
      AMsg := SInputRequired;
      exit;
    end;
    if not TryStrToFloat((AControl as TEdit).Text, value) then begin
      AMsg := SNumberRequired;
      exit;
    end;
    if value < 0 then begin
      AMsg := SPositiveNumberRequired;
      exit;
    end;
    if value = 0 then begin
      AMsg := SMustNotBeZero;
      exit;
    end;
  end;
  Result := true;
end;

function TBaseFrame.IsValidComboValue(AControl: TWinControl; out AMsg: String): Boolean;
begin
  Result := false;
  if (AControl is TCombobox) then begin
    if (AControl as TCombobox).ItemIndex < 0 then begin
      AMsg := SInputRequired;
      exit;
    end;
  end;
  Result := true;
end;

procedure TBaseFrame.ReadFromIni(ini: TCustomIniFile);
begin
end;

procedure TBaseFrame.SetEditLeft(AValue: Integer);
begin
  if AValue = FEditLeft then exit;
  FEditLeft := AValue;
end;

procedure TBaseFrame.UnitsChanged(Sender: TObject);
begin
  Calculate;
end;

function TBaseFrame.ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
begin
  Result := true;
end;

procedure TBaseFrame.WriteToIni(ini: TCustomIniFile);
begin
end;
end.


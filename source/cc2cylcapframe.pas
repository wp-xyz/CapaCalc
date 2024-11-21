unit cc2CylCapFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, SysUtils, Math, FileUtil, Forms, Controls, Dialogs,
  ccCylPlaneCapFrame;

type
  TTwoCylCapFrame = class(TCylPlaneCapFrame)
  private
    { private declarations }
  protected
    function DoCalc(d, R, L, eps: Extended): Extended; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    function ValidData(out {%H-}AMsg: String; out {%H-}AControl: TWinControl): Boolean; override;
  end;

implementation

{$R *.lfm}

uses
  ccGlobal, ccStrings;

constructor TTwoCylCapFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIniKey := '2 cylinders';
end;

function TTwoCylCapFrame.DoCalc(d, R, L, eps: Extended): Extended;
begin
  Result := PI * eps0 * eps * L / ln( (d/2 + sqrt(d*d*0.25 - R*R))/R);
end;

function TTwoCylCapFrame.ValidData(out AMsg: String; out AControl: TWinControl): Boolean;
var
  R, d: Extended;
begin
  Result := inherited;
  if Result then
  begin
    Result := false;
    R := StrToFloat(EdRadius.Text);
    d := StrToFloat(EdDist.Text);
    if (d/2 < R) or SameValue(d/2, R) then
    begin
      AControl := EdDist;
      AMsg := SRadiusNotLargerThanHalfDistance;
      exit;
    end;
    if (d/2 + sqrt(d*d/4 - R*R))/R <= 0 then
    begin
      AMsg := SParameterError;
      exit;
    end;
    Result := true;
  end;
end;

end.


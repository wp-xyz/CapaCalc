unit ccStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SAccuracy2Percent = 'Accuracy ca. 2%';
  SAccuracy6Percent = 'Accuracy is about 6%.';
  SCylRadiiMustNotBeEqual = 'The cylinder radii must not be equal.';
  SInaccurate = 'Inaccurate results.'#13'At least B/d>0.3 and H/d<10 required for 6% error.';
  SInputRequired = 'Input required.';
  SInnerRadiusMustBeSmaller = 'The inner radius must be smaller than the outer radius.';
  SMustNotBeZero = 'This input must not be zero.';
  SNumberRequired = 'Number required in this field.';
  SPositiveNumberRequired = 'Positive number required.';
  SRadiusNotLargerThanCenterPlaneDistance = 'Radius cannot be larger than center-to-plane distance.';
  SRadiusNotLargerThanHalfDistance = 'Radius cannot be larger than half-distance.';
  SSphereRadiiMustNotBeEqual = 'The sphere radii must not be equal.';
  STemperatureDependenceNotSupported = 'Temperature dependence of this material not supported.';
  STemperaturePositive = 'Temperature in Kelvin must be a positive number.';
  SVoltageMustBeSmallerThanBuiltIn = 'The applied voltage must be smaller then the built-in voltage (%f V)';
  SParameterError = 'Parameter error';

implementation

end.


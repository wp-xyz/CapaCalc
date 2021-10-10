unit ccGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLenUnits = (lu_nm, lu_Microns, lu_cm, lu_m);
  TAreaUnits = (au_nm2, au_Microns2, au_cm2, au_m2);
  TVolumeUnits = (vu_nm3, vu_Microns3, vu_cm3, vu_m3);
  TCapaUnits = (cu_aF, cu_fF, cu_pF, cu_nF, cu_F);
  TEFieldUnits = (eu_mV_nm, eu_V_micron, eu_kV_cm, eu_MV_m, eu_V_m);
  TTempUnits = (tuK, tuC);
  TMaterial = (mSi, mGaAs, mGe, mInP);

const
  CapaExpFormat = '0.000E+00';
  CapaStdFormat = '0.000';
  CapaPerAreaFormat = '0.000E+00';
  CapaPerLengthFormat = '0.000E+00';
  ConcFormat = '0.00E+00';

  LenStdFormat = '0.00';
  AreaStdFormat = '0.00E+00';

  eps0 = 8.854187817E-12;  // F/m
  q = 1.60217733E-19;      // C
  kB = 1.380658E-23;       // Boltzmann constant (J/k)

  Pi    = 3.1415926535897932384626;
  TwoPi = 6.2831853071795864769253;      // 2*Pi

  LenFactor    : array[TLenUnits] of double    = ( 1E-9,  1E-6,  1E-2, 1.0);
  AreaFactor   : array[TAreaUnits] of double   = ( 1E-18, 1E-12, 1E-4, 1.0);
  VolumeFactor : array[TVolumeUnits] of double = ( 1E-27, 1E-18, 1E-6, 1.0);
  CapaFactor   : array[TCapaUnits] of double   = ( 1E-18, 1E-15, 1E-12, 1E-9, 1.0 );
  EFieldFactor : array[TEFieldUnits] of double = ( 1E-3/1E-9, 1.0/1E-6, 1E3/1E-2, 1E6/1, 1.0 );
  MaterialEps  : array [TMaterial] of double   = (11.9*eps0, 12.4*eps0, 16.0*eps0, 12.5*eps0);
  ni           : array [TMaterial] of double   = (9.65e9, 2.25e6, 2e13, 1.3e7);  // 1/cm3

implementation

end.


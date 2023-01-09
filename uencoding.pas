unit UEncoding;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

procedure Encode(const InputStream, OutputStream: TStream);

implementation

uses
  ULZMAEncoder,
  ULZMACommon;

procedure Encode(const InputStream, OutputStream: TStream);
const
 {set Match Finder. Default: bt4.
  Algorithms from hc* group doesn't provide good compression
  ratio, but they often works pretty fast in combination with
  fast mode (-a0).

  Memory requirements depend from dictionary size
  (parameter "d" in table below).

   MF_ID     Memory                   Description

    bt2    d *  9.5 + 4MB  Binary Tree with 2 bytes hashing.
    bt3    d * 11.5 + 4MB  Binary Tree with 3 bytes hashing.
    bt4    d * 11.5 + 4MB  Binary Tree with 4 bytes hashing.
    hc4    d *  7.5 + 4MB  Hash Chain with 4 bytes hashing.}
  MatchFinder = 1;

 {Sets Dictionary size - [0, 30], default: 23 (8MB)
  The maximum value for dictionary size is 1 GB = 2^30 bytes.
  Dictionary size is calculated as DictionarySize = 2^N bytes.
  For decompressing file compressed by LZMA method with dictionary
  size D = 2^N you need about D bytes of memory (RAM).}
  DictionarySize = 1 shl 23;

 {set number of fast bytes - [5, 273], default: 128
  Usually big number gives a little bit better compression ratio
  and slower compression process.}
  FastBytes = 128;

 {set number of literal context bits - [0, 8], default: 3
  Sometimes lc=4 gives gain for big files.}
  LiteralContextBits = 3;

 {set number of literal pos bits - [0, 4], default: 0
  lp switch is intended for periodical data when period is
  equal 2^N. For example, for 32-bit (4 bytes)
  periodical data you can use lp=2. Often it's better to set lc0,
  if you change lp switch.}
  LiteralPositionBits = 0;

 {set number of pos bits - [0, 4], default: 2
  pb switch is intended for periodical data
  when period is equal 2^N.}
  PositionBits = 2;

 {write End Of Stream marker. By default LZMA doesn't write
  eos marker, since LZMA decoder knows uncompressed size
  stored in .lzma file header.}
  EndOfStreamMarker = False;
var
  Encoder: TLZMAEncoder;
  Index: Uint8;
begin
  try
    Encoder := TLZMAEncoder.Create;
    Encoder.SetDictionarySize(DictionarySize);
    Encoder.SeNumFastBytes(FastBytes);
    Encoder.SetMatchFinder(MatchFinder);
    Encoder.SetLcLpPb(LiteralContextBits, LiteralPositionBits, PositionBits);
    Encoder.SetEndMarkerMode(EndOfStreamMarker);
    Encoder.WriteCoderProperties(OutputStream);
    for Index := 0 to 7 do
      WriteByte(OutputStream, (InputStream.Size shr (8 * Index)) and $FF);
    Encoder.Code(InputStream, OutputStream, -1, -1);
  finally
    Encoder.Free;
  end;
end;

end.

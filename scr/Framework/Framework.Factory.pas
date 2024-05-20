unit Framework.Factory;

interface

uses
  System.Classes, System.SysUtils, System.NetEncoding, System.Hash, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, Vcl.Forms, Vcl.StdCtrls, Vcl.Mask,
  Vcl.ExtCtrls, Vcl.DBCtrls;

type
  TStatus    = (dsBrowse, dsDelete, dsEdit, dsInactive, dsInsert);
  TCondition = (aEnable, aDisable);

  TFactory = class
  private

  public
    function GetCurrentTimeStamp(AConnection : TFDConnection) : TDateTime;
    function GetNextID(ASequenceName : String; AConnection : TFDConnection) : Integer;
    function DateTimeIsNull(ADate: TDateTime): Boolean;
    function GetStrHashSHA512_256(AValue: String): String;
    function IsValideCNPJ(AValue : WideString) : boolean;
    procedure ClearControls(aView : TForm);
    procedure ControlControls(aView : TForm; aStatus : TStatus);
    procedure EnableDisableControls(aView : TForm; aCondition : TCondition);
    procedure FormatCurrecyValue(Sender: TObject; var Key: Word; Shift: TShiftState);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TFactory }

procedure TFactory.ClearControls(aView: TForm);
var
  aIndex: Integer;
begin
  try
    for aIndex := 0 to Pred(aView.ComponentCount) do
    begin
      if aView.Components[aIndex] is TEdit then
        (aView.Components[aIndex] as TEdit).Clear;
      if aView.Components[aIndex] is TMaskEdit then
        (aView.Components[aIndex] as TMaskEdit).Clear;
      if aView.Components[aIndex] is TComboBox then
        (aView.Components[aIndex] as TComboBox).ItemIndex := -1;
      if aView.Components[aIndex] is TDBLookupComboBox then
        (aView.Components[aIndex] as TDBLookupComboBox).KeyValue := -1;
    end;
  except
    on e: Exception do
      raise Exception.Create(e.Message);
  end;
end;

procedure TFactory.ControlControls(aView: TForm; aStatus: TStatus);
var
  aComponent : TComponent;
begin
  case aStatus of
    dsBrowse:
      begin
        aComponent := aView.FindComponent('btnNovo');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnEditar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnGravar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnCancelar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnApagar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnFechar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnVoltar');
        TButton(aComponent).Enabled := True;
      end;
    dsDelete:
      begin
        aComponent := aView.FindComponent('btnNovo');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnEditar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnGravar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnCancelar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnApagar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnFechar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnVoltar');
        TButton(aComponent).Enabled := True;
      end;
    dsEdit:
      begin
        aComponent := aView.FindComponent('btnNovo');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnEditar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnGravar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnCancelar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnApagar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnFechar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnVoltar');
        TButton(aComponent).Enabled := False;
      end;
    dsInactive:
      begin
        aComponent := aView.FindComponent('btnNovo');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnEditar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnGravar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnCancelar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnApagar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnFechar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnVoltar');
        TButton(aComponent).Enabled := True;
      end;
    dsInsert:
      begin
        aComponent := aView.FindComponent('btnNovo');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnEditar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnGravar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnCancelar');
        TButton(aComponent).Enabled := True;
        aComponent := aView.FindComponent('btnApagar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnFechar');
        TButton(aComponent).Enabled := False;
        aComponent := aView.FindComponent('btnVoltar');
        TButton(aComponent).Enabled := False;
      end;
  end;
end;

constructor TFactory.Create;
begin

end;

function TFactory.DateTimeIsNull(ADate: TDateTime): Boolean;
begin
  Result := ADate = Default(TDateTime);
end;

destructor TFactory.Destroy;
begin

  inherited;
end;

procedure TFactory.EnableDisableControls(aView: TForm; aCondition: TCondition);
var
  aIndex: Integer;
begin
  for aIndex := 0 to Pred(aView.ComponentCount) do
  begin
    if aView.Components[aIndex] is TEdit then
    begin
      if aCondition = aEnable then
        TEdit(aView.Components[aIndex]).Enabled := True
      else
        TEdit(aView.Components[aIndex]).Enabled := False;
    end else if aView.Components[aIndex] is TMaskEdit then
    begin
      if aCondition = aEnable then
        TMaskEdit(aView.Components[aIndex]).Enabled := True
      else
        TMaskEdit(aView.Components[aIndex]).Enabled := False;
    end else if aView.Components[aIndex] is TComboBox then
    begin
      if aCondition = aEnable then
        TComboBox(aView.Components[aIndex]).Enabled := True
      else
        TComboBox(aView.Components[aIndex]).Enabled := False;
    end else if aView.Components[aIndex] is TDBLookupComboBox then
    begin
      if aCondition = aEnable then
        TDBLookupComboBox(aView.Components[aIndex]).Enabled := True
      else
        TDBLookupComboBox(aView.Components[aIndex]).Enabled := False;
    end else if aView.Components[aIndex] is TRadioGroup then
    begin
      if aCondition = aEnable then
        TRadioGroup(aView.Components[aIndex]).Enabled := True
      else
        TRadioGroup(aView.Components[aIndex]).Enabled := False;
    end;
  end;
end;

function TFactory.GetCurrentTimeStamp(AConnection: TFDConnection): TDateTime;
var
  LQuery : TFDQuery;
begin
  LQuery := TFDQuery.Create(nil);
  LQuery.Connection := AConnection;
  try
    LQuery.Open('SELECT CURRENT_TIMESTAMP FROM RDB$DATABASE;');
    Result := LQuery.Fields[0].AsDateTime;
  finally
    FreeAndNil(LQuery);
  end;
end;

function TFactory.GetNextID(ASequenceName: String; AConnection: TFDConnection): Integer;
var
  LQuery : TFDQuery;
begin
  LQuery := TFDQuery.Create(nil);
  LQuery.Connection := AConnection;
  try
    LQuery.Open('SELECT GEN_ID(' + ASequenceName + ', 1) FROM RDB$DATABASE;');
    Result := LQuery.Fields[0].AsInteger;
  finally
    FreeAndNil(LQuery);
  end;
end;

function TFactory.GetStrHashSHA512_256(AValue: String): String;
var
  HashSHA: THashSHA2;
begin
  HashSHA := THashSHA2.Create;
  HashSHA.GetHashString(AValue);
  Result := HashSHA.GetHashString(AValue, SHA512_256);
end;

function TFactory.IsValideCNPJ(AValue: WideString): boolean;
var
  LPosicao, LCodigo, Lindex, LSoma2, LDv, LDv_Informado : integer;
  LDigito : array[1..14] of Byte;
begin

  Result := False;

  //DV_Informado - dois ultimos digitos de verificação
  Val(copy(AValue, 13, 2), LDv_Informado, LCodigo);

  //Desmembra o número do CNPJ na matriz digito
  for Lindex := 1 to 12 do
  begin
    Val(copy(AValue, Lindex, 1), LDigito[Lindex], LCodigo);
  end;

  //Calcula o 13º digito de verificação
  LPosicao := 5;
  LSoma2   := 0;
  for Lindex := 1 to 4 do
  begin
    LSoma2   := LSoma2 + LDigito[Lindex] * LPosicao;
    LPosicao := LPosicao - 1;
  end;

  LPosicao := 9;
  for Lindex := 5 to 12 do
  begin
    LSoma2   := LSoma2 + LDigito[Lindex] * LPosicao;
    LPosicao := LPosicao - 1;
  end;

  LDigito[13] := LSoma2 mod 11;
  if (LDigito[13] < 2) then
    LDigito[13] := 0
  else
    LDigito[13] := 11 - LDigito[13];

  //Calcula o 14º digito de verificação
  LPosicao := 6;
  LSoma2 := 0;
  for Lindex := 1 to 5 do
  begin
    LSoma2 := LSoma2 + LDigito[Lindex] * LPosicao;
    LPosicao := LPosicao - 1;
  end;

  LPosicao := 9;
  for Lindex := 6 to 13 do
  begin
    LSoma2 := LSoma2 + LDigito[Lindex] * LPosicao;
    LPosicao := LPosicao - 1;
  end;

  LDigito[14] := LSoma2 mod 11;
  if (LDigito[14] < 2) then
    LDigito[14] := 0
  else
    LDigito[14] := 11 - LDigito[14];

  //Verifica se DV calculado é igual a DV_Informado
  LDv := LDigito[13] * 10 + LDigito[14];
  if (LDv = LDv_Informado) then
    Result := True;
end;

procedure TFactory.FormatCurrecyValue(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  LValor: String;
begin
  //Metodo portado de uma função JavaScript por Autor desconhecido
  //diponível em: https://www.devmedia.com.br/quick-tips-formatando-valores-ao-digitar/14354
  if (Key in [96..107]) or (Key in [48..57]) then
  begin
    LValor := TEdit(Sender).Text;
    LValor := StringReplace(LValor, ',', '',[rfReplaceAll]);
    LValor := StringReplace(LValor, '.', '',[rfReplaceAll]);
    if Length(LValor) = 3 then
    begin
      LValor := Copy(LValor, 1, 1) + ',' +
                Copy(LValor, 2);
      TEdit(Sender).Text := LValor;
      TEdit(Sender).SelStart := Length(LValor);
    end else if (Length(LValor) > 3) and (Length(LValor) < 6) then
    begin
      LValor := Copy(LValor, 1, length(LValor) - 2) + ',' +
                Copy(LValor, length(LValor) - 1);
      TEdit(Sender).Text := LValor;
      TEdit(Sender).SelStart := Length(LValor);
    end else if (Length(LValor) >= 6) and (Length(LValor) < 9) then
    begin
      LValor := Copy(LValor, 1, length(LValor )- 5) + '.' +
                Copy(LValor, length(LValor) - 4, 3) + ',' +
                Copy(LValor, length(LValor) - 1);
      TEdit(Sender).Text := LValor;
      TEdit(Sender).SelStart := Length(LValor);
    end else if (Length(LValor) >= 9) and (Length(LValor) < 12) then
    begin
      LValor := Copy(LValor, 1, length(LValor) - 8) + '.' +
                Copy(LValor,length(LValor) - 7, 3) + '.' +
                Copy(LValor,length(LValor) - 4, 3) + ',' +
                Copy(LValor,length(LValor) - 1);
      TEdit(Sender).Text := LValor;
      TEdit(Sender).SelStart := Length(LValor);
    end else if (Length(LValor) >= 12) and (Length(LValor) < 15)  then
    begin
      LValor := Copy(LValor, 1, length(LValor) - 11) + '.' +
                Copy(LValor, length(LValor) - 10, 3) + '.' +
                Copy(LValor, length(LValor) - 7, 3)  + '.' +
                Copy(LValor, length(LValor) - 4, 3)  + ',' +
                Copy(LValor, length(LValor) - 1);
      TEdit(Sender).Text := LValor;
      TEdit(Sender).SelStart := Length(LValor);
    end;
  end;
end;

end.

unit Dao.PedidoItens;

interface

uses
  System.Classes, System.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, Dao.Conexao, Model.PedidoItens, Framework.Factory;

type
  TPedidoItensDao = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FFactory : TFactory;
  public
    function Insert(APedido_ItensModel : TPedidoItensModel) : Boolean;
    function Update(APedido_ItensModel : TPedidoItensModel) : Boolean;
    function Delete(co_item : Integer) : Boolean;
    function GetNewID(ASequenceName : String) : Double;
    function GetAll(nu_pedido : Integer): TObjectList<TPedidoItensModel>;
    function GetDateTime : TDateTime;
    procedure ToLoadComboProduto(AObject : TFDQuery);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPedido_ItensDao }

constructor TPedidoItensDao.Create;
begin
  Self.FStrB             := TStringBuilder.Create;
  Self.FQuery            := TFDQuery.Create(nil);
  Self.FQuery.Connection := DMConexao.Conexao;
  Self.FFactory          := TFactory.Create;
end;

function TPedidoItensDao.Delete(co_item: Integer): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM PEDIDO_ITENS    ')
            .AppendLine('  WHERE CO_ITEM = :CO_ITEM; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                         := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_ITEM').AsInteger := co_item;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    if Self.FQuery.RowsAffected = 0 then
      Result := False
    else
      Result := True;
  except
    on e : exception do
    begin
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

destructor TPedidoItensDao.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  inherited;
end;

function TPedidoItensDao.GetAll(nu_pedido: Integer): TObjectList<TPedidoItensModel>;
var
  LPedido_Itens : TPedidoItensModel;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_ITEM, NU_PEDIDO, CO_PRODUTO, NU_QUANTIDADE, ')
            .AppendLine('       VL_VENDA, VL_TOTAL, DT_CADASTRO                ')
            .AppendLine('  FROM PEDIDO_ITENS                                   ')
            .AppendLine('  WHERE NU_PEDIDO = :NU_PEDIDO                        ')
            .AppendLine('  ORDER BY CO_ITEM;                                   ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  Self.FQuery.ParamByName('NU_PEDIDO').AsInteger := nu_pedido;

  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;
  Result := TObjectList<TPedidoItensModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LPedido_Itens               := TPedidoItensModel.Create;
      LPedido_Itens.co_item       := Self.FQuery.FieldByName('CO_ITEM').AsInteger;
      LPedido_Itens.nu_pedido     := Self.FQuery.FieldByName('NU_PEDIDO').AsInteger;
      LPedido_Itens.co_produto    := Self.FQuery.FieldByName('CO_PRODUTO').AsInteger;
      LPedido_Itens.nu_quantidade := Self.FQuery.FieldByName('NU_QUANTIDADE').AsFloat;
      LPedido_Itens.vl_venda      := Self.FQuery.FieldByName('VL_VENDA').AsCurrency;
      LPedido_Itens.vl_total      := Self.FQuery.FieldByName('VL_TOTAL').AsCurrency;
      LPedido_Itens.dt_cadastro := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
      Result.Add(LPedido_Itens);
      Self.FQuery.Next;
    end;
  end;
end;

function TPedidoItensDao.GetDateTime: TDateTime;
begin
  Result := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
end;

function TPedidoItensDao.GetNewID(ASequenceName: String): Double;
begin
  Result := Self.FFactory.GetNextID(ASequenceName, DMConexao.Conexao);
end;

function TPedidoItensDao.Insert(APedido_ItensModel: TPedidoItensModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('INSERT INTO PEDIDO_ITENS (CO_ITEM, NU_PEDIDO, CO_PRODUTO,    ')
            .AppendLine('                          NU_QUANTIDADE, VL_VENDA)           ')
            .AppendLine('                  VALUES (:CO_ITEM, :NU_PEDIDO, :CO_PRODUTO, ')
            .AppendLine('                          :NU_QUANTIDADE, :VL_VENDA);        ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                             := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_ITEM').AsFloat       := APedido_ItensModel.co_item;
  Self.FQuery.ParamByName('NU_PEDIDO').AsLargeInt  := APedido_ItensModel.nu_pedido;
  Self.FQuery.ParamByName('CO_PRODUTO').AsInteger  := APedido_ItensModel.co_produto;
  Self.FQuery.ParamByName('NU_QUANTIDADE').AsFloat := APedido_ItensModel.nu_quantidade;
  Self.FQuery.ParamByName('VL_VENDA').AsCurrency   := APedido_ItensModel.vl_venda;

  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    if Self.FQuery.RowsAffected = 0 then
      Result := False
    else
      Result := True;
  except
    on e : exception do
    begin
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

procedure TPedidoItensDao.ToLoadComboProduto(AObject: TFDQuery);
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_PRODUTO, TX_DESCRICAO, VL_VENDA ')
            .AppendLine('  FROM PRODUTO                            ')
            .AppendLine('  ORDER BY TX_DESCRICAO                   ');

  AObject.Close;
  AObject.SQL.Clear;
  AObject.Connection := DMConexao.Conexao;
  AObject.SQL.Text   := Self.FStrB.ToString;

  if not AObject.Connection.InTransaction then
    AObject.Connection.StartTransaction;
  try
    AObject.Open;
    AObject.Connection.Commit;
  except
    AObject.Connection.Rollback;
  end;
end;

function TPedidoItensDao.Update(APedido_ItensModel: TPedidoItensModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('UPDATE PEDIDO_ITENS                   ')
            .AppendLine('  SET NU_QUANTIDADE  = :NU_QUANTIDADE ')
            .AppendLine('  WHERE CO_ITEM = :CO_ITEM;    ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                             := Self.FStrB.ToString;
  Self.FQuery.ParamByName('NU_QUANTIDADE').AsFloat := APedido_ItensModel.nu_quantidade;
  Self.FQuery.ParamByName('CO_ITEM').AsInteger     := APedido_ItensModel.co_item;

  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.ExecSQL;
    Self.FQuery.Connection.Commit;
    if Self.FQuery.RowsAffected = 0 then
      Result := False
    else
      Result := True;
  except
    on e : exception do
    begin
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

end.

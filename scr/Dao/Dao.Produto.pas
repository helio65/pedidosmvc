unit Dao.Produto;

interface

uses
  System.Classes, System.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, Dao.Conexao, Model.Produto, Framework.Factory;

type
  TProdutoDao = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FFactory : TFactory;
  public
    function Insert(AProdutoModel : TProdutoModel) : Boolean;
    function Update(AProdutoModel : TProdutoModel) : Boolean;
    function Delete(co_produto : Integer) : Boolean;
    function GetNewID(ASequenceName : String) : Double;
    function GetAll: TObjectList<TProdutoModel>;
    function GetDateTime : TDateTime;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProdutoDao }

constructor TProdutoDao.Create;
begin
  Self.FStrB             := TStringBuilder.Create;
  Self.FQuery            := TFDQuery.Create(nil);
  Self.FQuery.Connection := DMConexao.Conexao;
  Self.FFactory          := TFactory.Create;
end;

function TProdutoDao.Delete(co_produto: Integer): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('DELETE FROM PRODUTO               ')
            .AppendLine('  WHERE CO_PRODUTO = :CO_PRODUTO; ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                            := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_PRODUTO').AsInteger := co_produto;
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

destructor TProdutoDao.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  inherited;
end;

function TProdutoDao.GetAll: TObjectList<TProdutoModel>;
var
  LProdutos : TProdutoModel;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_PRODUTO, TX_DESCRICAO, VL_VENDA, NU_ESTOQUE, ')
            .AppendLine('       DT_CADASTRO, DT_ALTERACAO                       ')
            .AppendLine('  FROM PRODUTO                                         ')
            .AppendLine('  ORDER BY TX_DESCRICAO;                               ');

  Self.FQuery.Close;
  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text := Self.FStrB.ToString;
  if not Self.FQuery.Connection.InTransaction then
    Self.FQuery.Connection.StartTransaction;
  try
    Self.FQuery.Open;
    Self.FQuery.Connection.Commit;
  except
    Self.FQuery.Connection.Rollback;
  end;
  Result := TObjectList<TProdutoModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LProdutos              := TProdutoModel.Create;
      LProdutos.co_produto   := Self.FQuery.FieldByName('CO_PRODUTO').AsInteger;
      LProdutos.tx_descricao := Self.FQuery.FieldByName('TX_DESCRICAO').AsString;
      LProdutos.vl_venda     := Self.FQuery.FieldByName('VL_VENDA').AsCurrency;
      LProdutos.nu_estoque   := Self.FQuery.FieldByName('NU_ESTOQUE').AsFloat;
      LProdutos.dt_cadastro  := Self.FQuery.FieldByName('DT_CADASTRO').AsDateTime;
      if not Self.FQuery.FieldByName('DT_ALTERACAO').IsNull then
        LProdutos.dt_alteracao    := Self.FQuery.FieldByName('DT_ALTERACAO').AsDateTime;
      Result.Add(LProdutos);
      Self.FQuery.Next;
    end;
  end;
end;

function TProdutoDao.GetDateTime: TDateTime;
begin
  Result := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
end;

function TProdutoDao.GetNewID(ASequenceName: String): Double;
begin
  Result := Self.FFactory.GetNextID(ASequenceName, DMConexao.Conexao);
end;

function TProdutoDao.Insert(AProdutoModel: TProdutoModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('INSERT INTO PRODUTO (CO_PRODUTO, TX_DESCRICAO, VL_VENDA, NU_ESTOQUE)      ')
            .AppendLine('             VALUES (:CO_PRODUTO, :TX_DESCRICAO, :VL_VENDA, :NU_ESTOQUE); ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                             := Self.FStrB.ToString;
  Self.FQuery.ParamByName('CO_PRODUTO').AsInteger  := AProdutoModel.co_produto;
  Self.FQuery.ParamByName('TX_DESCRICAO').AsString := AProdutoModel.tx_descricao;
  Self.FQuery.ParamByName('VL_VENDA').AsCurrency   := AProdutoModel.vl_venda;
  Self.FQuery.ParamByName('NU_ESTOQUE').AsFloat    := AProdutoModel.nu_estoque;

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

function TProdutoDao.Update(AProdutoModel: TProdutoModel): Boolean;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('UPDATE PRODUTO                      ')
            .AppendLine('  SET TX_DESCRICAO = :TX_DESCRICAO, ')
            .AppendLine('      VL_VENDA     = :VL_VENDA,     ')
            .AppendLine('      NU_ESTOQUE   = :NU_ESTOQUE,   ')
            .AppendLine('      DT_ALTERACAO = :DT_ALTERACAO  ')
            .AppendLine('  WHERE CO_PRODUTO = :CO_PRODUTO;   ');

  Self.FQuery.SQL.Clear;
  Self.FQuery.SQL.Text                               := Self.FStrB.ToString;
  Self.FQuery.ParamByName('TX_DESCRICAO').AsString   := AProdutoModel.tx_descricao;
  Self.FQuery.ParamByName('VL_VENDA').AsCurrency     := AProdutoModel.vl_venda;
  Self.FQuery.ParamByName('NU_ESTOQUE').AsFloat      := AProdutoModel.nu_estoque;
  Self.FQuery.ParamByName('DT_ALTERACAO').AsDateTime := AProdutoModel.dt_alteracao;
  Self.FQuery.ParamByName('CO_PRODUTO').AsInteger    := AProdutoModel.co_produto;

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

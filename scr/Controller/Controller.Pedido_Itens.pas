unit Controller.Pedido_Itens;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, Framework.Factory, Model.Pedido_Itens,
  View.Pedido_Itens, FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao,
  Vcl.Dialogs, System.Variants, Data.DB;

type
  TPedido_ItensController = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FView : TFrmPedido_Itens;
    FFactory : TFactory;
    FStatus : TStatus;
    FPedido_ItensModel : TPedido_ItensModel;
    FNu_pedido : Integer;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure gridBaseDblClick(Sender: TObject);
    procedure FDMPedido_ItensAfterScroll(DataSet: TDataSet);
    procedure FDQProdutosAfterScroll(DataSet: TDataSet);
    procedure edtQuantidadeExit(Sender: TObject);
    procedure edtCodigoProdutoExit(Sender: TObject);
    procedure ToLoadComboProduto;
  public
    procedure Insert;
    function Delete(co_item : Integer) : Boolean;
    procedure Save(aObject : TPedido_ItensModel);
    function GetAll(nu_pedido : Integer) : TObjectList<TPedido_ItensModel>;
    constructor Create(nu_pedido : Integer);
    destructor Destroy; override;
  end;

implementation

{ TPedido_ItensController }

procedure TPedido_ItensController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão deste Produto do Pedido?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.Delete(Self.FView.FDMPedido_ItensCO_ITEM.AsInteger) then
    begin
      Self.FFactory.ClearControls(Self.FView);
      Self.FormShow(Sender);
    end
    else
      Self.FormShow(Sender);
  end;
end;

procedure TPedido_ItensController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigoProduto.Text := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger);
    Self.FView.comboProduto.KeyValue := Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger;
    Self.FView.edtQuantidade.Text    := FloatToStr(Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat);
    Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency);
    Self.FView.edtValorTotal.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency);
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end else if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FStatus := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end;
end;

procedure TPedido_ItensController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtCodigoProduto.Text := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger);
  Self.FView.comboProduto.KeyValue := Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger;
  Self.FView.edtQuantidade.Text    := FloatToStr(Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat);
  Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency);
  Self.FView.edtValorTotal.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency);
end;

procedure TPedido_ItensController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TPedido_ItensController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.comboProduto.Text).IsEmpty then
      aListError.Add('Produto');
    if String(Self.FView.edtQuantidade.Text).IsEmpty then
      aListError.Add('Quantidade');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.comboProduto.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
        Self.FPedido_ItensModel.nu_pedido := Self.FView.FDMPedido_ItensNU_PEDIDO.AsInteger
      else
        Self.FPedido_ItensModel.nu_pedido   := Self.FNu_pedido;
      Self.FPedido_ItensModel.co_produto    := Self.FView.comboProduto.KeyValue;
      Self.FPedido_ItensModel.nu_quantidade := StrToFloatDef(Self.FView.edtQuantidade.Text, 1);
      Self.FPedido_ItensModel.vl_venda      := StrToCurr(String(Self.FView.edtValorVenda.Text).Replace('.', ''));
      Self.Save(Self.FPedido_ItensModel);
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TPedido_ItensController.btnNovoClick(Sender: TObject);
begin
  Self.Insert;
end;

constructor TPedido_ItensController.Create(nu_pedido: Integer);
begin
  Self.FNu_pedido                        := nu_pedido;
  Self.FStrB                             := TStringBuilder.Create;
  Self.FQuery                            := TFDQuery.Create(nil);
  Self.FQuery.Connection                 := DMConexao.Conexao;
  Self.FFactory                          := TFactory.Create;
  Self.FPedido_ItensModel                := TPedido_ItensModel.Create;
  Self.FView                             := TFrmPedido_Itens.Create(nil);
  Self.FView.KeyPreview                  := True;
  Self.FView.btnFechar.OnClick           := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick             := Self.btnNovoClick;
  Self.FView.btnEditar.OnClick           := Self.btnEditarClick;
  Self.FView.btnGravar.OnClick           := Self.btnGravarClick;
  Self.FView.OnShow                      := Self.FormShow;
  Self.FView.OnKeyPress                  := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick         := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick           := Self.btnApagarClick;
  Self.FView.gridBase.OnDblClick         := Self.gridBaseDblClick;
  Self.FView.FDMPedido_Itens.AfterScroll := Self.FDMPedido_ItensAfterScroll;
  Self.FView.FDQProdutos.AfterScroll     := Self.FDQProdutosAfterScroll;
  Self.FView.edtQuantidade.OnExit        := Self.edtQuantidadeExit;
  Self.FView.edtCodigoProduto.OnExit     := Self.edtCodigoProdutoExit;
  Self.FView.btnNovo.ShowHint            := True;
  Self.FView.btnNovo.Hint                := 'Adicionar um novo registro';
  Self.FView.btnEditar.ShowHint          := True;
  Self.FView.btnEditar.Hint              := 'Editar o registro corrente';
  Self.FView.btnGravar.ShowHint          := True;
  Self.FView.btnGravar.Hint              := 'Gravar o registro corrente';
  Self.FView.btnCancelar.ShowHint        := True;
  Self.FView.btnCancelar.Hint            := 'Cnacelar a operação corrente';
  Self.FView.btnApagar.ShowHint          := True;
  Self.FView.btnApagar.Hint              := 'Apagar o registro corrente';
  Self.FView.btnFechar.ShowHint          := True;
  Self.FView.btnFechar.Hint              := 'Fechar';
  Self.FStatus                           := TStatus.dsInactive;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FView.ShowModal;
end;

function TPedido_ItensController.Delete(co_item: Integer): Boolean;
begin
  Self.FStatus := TStatus.dsDelete;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
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
    Result := True;
  except
    on e : exception do
    begin
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FQuery.Connection.Rollback;
      raise Exception.Create(e.Message);
    end;
  end;
end;

destructor TPedido_ItensController.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FPedido_ItensModel);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TPedido_ItensController.edtCodigoProdutoExit(Sender: TObject);
begin
  if (Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert) then
  begin
    if not String(Self.FView.edtCodigoProduto.Text).IsEmpty then
    begin
      if not Self.FView.FDQProdutos.Locate('CO_PRODUTO', Self.FView.edtCodigoProduto.Text, []) then
      begin
        Application.MessageBox('Produto não cadastro, verifique!', 'Atenção', MB_OK+MB_ICONWARNING);
        Self.FView.edtCodigoProduto.SelectAll;
        Self.FView.edtCodigoProduto.SetFocus;
        Exit;
      end
      else
      begin
        Self.FView.comboProduto.KeyValue := Self.FView.edtCodigoProduto.Text;
        Self.FView.edtQuantidade.SetFocus;
      end;
    end;
  end;
end;

procedure TPedido_ItensController.edtQuantidadeExit(Sender: TObject);
var
  LQuantidade : Double;
  LValor : Currency;
begin
  if (Self.FStatus = TStatus.dsEdit) or (Self.FStatus = TStatus.dsInsert) then
  begin
    if not String(Self.FView.edtQuantidade.Text).IsEmpty then
    begin
      LQuantidade := StrToFloatDef(Self.FView.edtQuantidade.Text, 1);
      LValor := StrToCurr(String(Self.FView.edtValorVenda.Text).Replace('.', ''));
      Self.FView.edtValorTotal.Text := FormatFloat('#,##0.00', (LValor * LQuantidade));
    end;
  end;
end;

procedure TPedido_ItensController.FDMPedido_ItensAfterScroll(DataSet: TDataSet);
begin
  if Self.FStatus <> TStatus.dsBrowse then
    Exit;
  Self.FView.edtCodigoProduto.Text := FormatFloat('000000', Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger);
  Self.FView.comboProduto.KeyValue := Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger;
  Self.FView.edtQuantidade.Text    := FloatToStr(Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat);
  Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency);
  Self.FView.edtValorTotal.Text    := FormatFloat('#,##0.00', Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency);
end;

procedure TPedido_ItensController.FDQProdutosAfterScroll(DataSet: TDataSet);
begin
  if (not (String(Self.FView.comboProduto.Text).IsEmpty)) or (not (String(Self.FView.edtCodigoProduto.Text).IsEmpty))  then
  begin
    Self.FView.edtCodigoProduto.Text := FormatFloat('000000', DataSet.FieldByName('CO_PRODUTO').AsInteger);
    Self.FView.edtValorVenda.Text    := FormatFloat('#,##0.00', DataSet.FieldByName('VL_VENDA').AsCurrency);
  end;
end;

procedure TPedido_ItensController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TPedido_ItensController.FormShow(Sender: TObject);
var
  LPedido_Itens : TObjectList<TPedido_ItensModel>;
  aIndex: Integer;
begin
  Self.FView.Caption := 'Cadastro de Itens do Pedido nº ' + FormatFloat('000000', Self.FNu_pedido);
  LPedido_Itens := Self.GetAll(Self.FNu_pedido);
  try
    if LPedido_Itens.Count > 0 then
    begin
      Self.FView.FDMPedido_Itens.Open;
      Self.FView.FDMPedido_Itens.EmptyDataSet;
      for aIndex := 0 to Pred(LPedido_Itens.Count) do
      begin
        Self.FView.FDMPedido_Itens.Insert;
        Self.FView.FDMPedido_ItensNU_PEDIDO.AsInteger    := LPedido_Itens[aIndex].nu_pedido;
        Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger   := LPedido_Itens[aIndex].co_produto;
        Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat  := LPedido_Itens[aIndex].nu_quantidade;
        Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency    := LPedido_Itens[aIndex].vl_venda;
        Self.FView.FDMPedido_ItensVL_TOTAL.AsCurrency    := LPedido_Itens[aIndex].vl_total;
        Self.FView.FDMPedido_ItensDT_CADASTRO.AsDateTime := LPedido_Itens[aIndex].dt_cadastro;
        Self.FView.FDMPedido_Itens.Post;
      end;
      Self.FView.FDMPedido_Itens.First;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMPedido_Itens.Close;
    end;
    Self.ToLoadComboProduto;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LPedido_Itens);
  end;
end;

function TPedido_ItensController.GetAll(nu_pedido: Integer): TObjectList<TPedido_ItensModel>;
var
  LPedido_Itens : TPedido_ItensModel;
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
  Result := TObjectList<TPedido_ItensModel>.Create;
  if Self.FQuery.RecordCount > 0 then
  begin
    while not Self.FQuery.Eof do
    begin
      LPedido_Itens               := TPedido_ItensModel.Create;
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

procedure TPedido_ItensController.gridBaseDblClick(Sender: TObject);
begin
  Self.btnEditarClick(Sender);
end;

procedure TPedido_ItensController.Insert;
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtCodigoProduto.SetFocus;
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TPedido_ItensController.Save(aObject: TPedido_ItensModel);
begin
  if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FStrB.Clear;
    Self.FStrB.AppendLine('INSERT INTO PEDIDO_ITENS (CO_ITEM, NU_PEDIDO, CO_PRODUTO,    ')
              .AppendLine('                          NU_QUANTIDADE, VL_VENDA)           ')
              .AppendLine('                  VALUES (:CO_ITEM, :NU_PEDIDO, :CO_PRODUTO, ')
              .AppendLine('                          :NU_QUANTIDADE, :VL_VENDA);        ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                             := Self.FStrB.ToString;
    Self.FQuery.ParamByName('CO_ITEM').AsInteger     := Self.FFactory.GetNextID('INC_CO_ITEM', DMConexao.Conexao);
    Self.FQuery.ParamByName('NU_PEDIDO').AsInteger   := aObject.nu_pedido;
    Self.FQuery.ParamByName('CO_PRODUTO').AsInteger  := aObject.co_produto;
    Self.FQuery.ParamByName('NU_QUANTIDADE').AsFloat := aObject.nu_quantidade;
    Self.FQuery.ParamByName('VL_VENDA').AsCurrency   := aObject.vl_venda;

    if not Self.FView.FDMPedido_Itens.Active then
      Self.FView.FDMPedido_Itens.Open;

    Self.FView.FDMPedido_Itens.Insert;
    Self.FView.FDMPedido_ItensCO_ITEM.AsInteger     := aObject.co_item;
    Self.FView.FDMPedido_ItensNU_PEDIDO.AsInteger   := aObject.nu_pedido;
    Self.FView.FDMPedido_ItensCO_PRODUTO.AsInteger  := aObject.co_produto;
    Self.FView.FDMPedido_ItensNU_QUANTIDADE.AsFloat := aObject.nu_quantidade;
    Self.FView.FDMPedido_ItensVL_VENDA.AsCurrency   := aObject.vl_venda;
    Self.FView.FDMPedido_Itens.Post;

    if not Self.FQuery.Connection.InTransaction then
      Self.FQuery.Connection.StartTransaction;
    try
      Self.FQuery.ExecSQL;
      Self.FQuery.Connection.Commit;
    except
      on e : exception do
      begin
        Self.FQuery.Connection.Rollback;
        raise Exception.Create(e.Message);
      end;
    end;
  end else if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FStrB.Clear;
    Self.FStrB.AppendLine('UPDATE PEDIDO_ITENS                   ')
              .AppendLine('  SET NU_QUANTIDADE  = :NU_QUANTIDADE ')
              .AppendLine('  WHERE CO_ITEM = :CO_ITEM;    ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                             := Self.FStrB.ToString;
    Self.FQuery.ParamByName('NU_QUANTIDADE').AsFloat := aObject.nu_quantidade;
    Self.FQuery.ParamByName('CO_ITEM').AsInteger     := aObject.co_item;

    if not Self.FQuery.Connection.InTransaction then
      Self.FQuery.Connection.StartTransaction;
    try
      Self.FQuery.ExecSQL;
      Self.FQuery.Connection.Commit;
    except
      on e : exception do
      begin
        Self.FQuery.Connection.Rollback;
        raise Exception.Create(e.Message);
      end;
    end;
  end;
end;

procedure TPedido_ItensController.ToLoadComboProduto;
begin
  Self.FStrB.Clear;
  Self.FStrB.AppendLine('SELECT CO_PRODUTO, TX_DESCRICAO, VL_VENDA ')
            .AppendLine('  FROM PRODUTO                            ')
            .AppendLine('  ORDER BY TX_DESCRICAO                   ');

  Self.FView.FDQProdutos.Close;
  Self.FView.FDQProdutos.SQL.Clear;
  Self.FView.FDQProdutos.Connection := DMConexao.Conexao;
  Self.FView.FDQProdutos.SQL.Text   := Self.FStrB.ToString;

  if not Self.FView.FDQProdutos.Connection.InTransaction then
    Self.FView.FDQProdutos.Connection.StartTransaction;
  try
    Self.FView.FDQProdutos.Open;
    Self.FView.FDQProdutos.Connection.Commit;
  except
    Self.FView.FDQProdutos.Connection.Rollback;
  end;
end;

end.

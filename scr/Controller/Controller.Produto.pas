unit Controller.Produto;

interface

uses
  Vcl.Forms, Winapi.Windows, Winapi.Messages, System.Classes, System.SysUtils,
  System.Generics.Collections, Framework.Factory, Model.Produto, View.Produto,
  FireDAC.Comp.Client, FireDAC.Stan.Param, Dao.Conexao,
  Vcl.Dialogs, System.Variants, Controller.Imagens;

type
  TProdutoController = class
  private
    FStrB : TStringBuilder;
    FQuery : TFDQuery;
    FView : TFrmProduto;
    FFactory : TFactory;
    FStatus : TStatus;
    FProdutoModel : TProdutoModel;
    FImagensController : TImagensController;
    procedure btnFecharClick(Sender : TObject);
    procedure btnNovoClick(Sender : TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGravarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnApagarClick(Sender: TObject);
    procedure btnVoltarClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure gridBaseDblClick(Sender: TObject);
    procedure edtValorKeyPress(Sender: TObject; var Key: Char);
    procedure edtValorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnImagensOnClick(Sender: TObject);
  public
    procedure Insert;
    function Delete(co_produto : Integer) : Boolean;
    procedure Save(aObject : TProdutoModel);
    function GetAll : TObjectList<TProdutoModel>;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TProdutoController }

procedure TProdutoController.btnApagarClick(Sender: TObject);
begin
  if Application.MessageBox('Confirma a exclusão deste Produto?', 'Atenção', MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2) = IDYES then
  begin
    Self.FStatus := TStatus.dsDelete;
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    if Self.Delete(Self.FView.FDMProdutoCO_PRODUTO.AsInteger) then
    begin
      if Self.FView.pgcPrincipal.ActivePage = Self.FView.tabCadastro then
      begin
        Self.FFactory.ClearControls(Self.FView);
        Self.FormShow(Sender);
      end else
      begin
        Self.FormShow(Sender);
      end;
    end else
    begin
      Self.FormShow(Sender);
    end;
  end;
end;

procedure TProdutoController.btnCancelarClick(Sender: TObject);
begin
  if Self.FStatus = TStatus.dsEdit then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FView.edtCodigo.Text      := FormatFloat('000000', Self.FView.FDMProdutoCO_PRODUTO.AsInteger);
    Self.FView.edtDescricao.Text   := Self.FView.FDMProdutoTX_DESCRICAO.AsString;
    Self.FView.edtValor.Text       := FormatFloat('#,##0.00', Self.FView.FDMProdutoVL_VENDA.AsCurrency);
    Self.FView.edtEstoque.Text     := FloatToStr(Self.FView.FDMProdutoNU_ESTOQUE.AsFloat);
    Self.FStatus                   := TStatus.dsBrowse;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end else if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FFactory.ClearControls(Self.FView);
    Self.FStatus                       := TStatus.dsBrowse;
    Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  end;
end;

procedure TProdutoController.btnEditarClick(Sender: TObject);
begin
  Self.FStatus := TStatus.dsEdit;
  if Self.FView.pgcPrincipal.ActivePage = Self.FView.tabConsulta then
    Self.gridBaseDblClick(Sender);
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
end;

procedure TProdutoController.btnFecharClick(Sender: TObject);
begin
  Self.FView.Close;
end;

procedure TProdutoController.btnGravarClick(Sender: TObject);
var
  aListError : TStringList;
begin
  aListError := TStringList.Create;
  aListError.Add('Lista de erro(s):');
  aListError.Add('');
  try
    if String(Self.FView.edtDescricao.Text).IsEmpty then
      aListError.Add('Descrição');
    if String(Self.FView.edtValor.Text).IsEmpty then
      aListError.Add('Valor');

    if aListError.Count > 2 then
    begin
      aListError.Add('');
      aListError.Add('O(s) campo(s) listado(s) acima são de preenchimento obrigatório.');

      Application.MessageBox(PChar(aListError.Text), 'Atenção', MB_OK+MB_ICONERROR);
      Self.FView.edtDescricao.SetFocus;
    end else
    begin
      if Self.FStatus = TStatus.dsEdit then
        Self.FProdutoModel.co_produto := Self.FView.FDMProdutoCO_PRODUTO.AsInteger
      else
        Self.FProdutoModel.co_produto := StrToInt(Self.FView.edtCodigo.Text);
      Self.FProdutoModel.tx_descricao := Self.FView.edtDescricao.Text;
      Self.FProdutoModel.vl_venda     := StrToCurr(String(Self.FView.edtValor.Text).Replace('.', ''));
      Self.FProdutoModel.nu_estoque   := StrToFloatDef(Self.FView.edtEstoque.Text, 0);
      Self.FProdutoModel.dt_alteracao := Self.FFactory.GetCurrentTimeStamp(DMConexao.Conexao);
      Self.Save(Self.FProdutoModel);
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FFactory.EnableDisableControls(Self.FView, aDisable);
    end;
  finally
    FreeAndNil(aListError);
  end;
end;

procedure TProdutoController.btnImagensOnClick(Sender: TObject);
begin
  if Self.FView.FDMProduto.RecordCount = 0 then
    Exit;
  try
    Self.FImagensController := TImagensController.Create(Self.FView.FDMProdutoCO_PRODUTO.AsInteger);
  finally
    FreeAndNil(Self.FImagensController);
  end;
end;

procedure TProdutoController.btnNovoClick(Sender: TObject);
begin
  Self.Insert;
end;

procedure TProdutoController.btnVoltarClick(Sender: TObject);
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
end;

constructor TProdutoController.Create;
begin
  Self.FStrB                      := TStringBuilder.Create;
  Self.FQuery                     := TFDQuery.Create(nil);
  Self.FQuery.Connection          := DMConexao.Conexao;
  Self.FFactory                   := TFactory.Create;
  Self.FProdutoModel              := TProdutoModel.Create;
  Self.FView                      := TFrmProduto.Create(nil);
  Self.FView.KeyPreview           := True;
  Self.FView.btnFechar.OnClick    := Self.btnFecharClick;
  Self.FView.btnNovo.OnClick      := Self.btnNovoClick;
  Self.FView.btnEditar.OnClick    := Self.btnEditarClick;
  Self.FView.btnGravar.OnClick    := Self.btnGravarClick;
  Self.FView.btnVoltar.OnClick    := Self.btnVoltarClick;
  Self.FView.btnImagens.OnClick   := Self.btnImagensOnClick;
  Self.FView.OnShow               := Self.FormShow;
  Self.FView.OnKeyPress           := Self.FormKeyPress;
  Self.FView.btnCancelar.OnClick  := Self.btnCancelarClick;
  Self.FView.btnApagar.OnClick    := Self.btnApagarClick;
  Self.FView.gridBase.OnDblClick  := Self.gridBaseDblClick;
  Self.FView.edtValor.OnKeyPress  := Self.edtValorKeyPress;
  Self.FView.edtValor.OnKeyUp     := Self.edtValorKeyUp;
  Self.FView.btnNovo.ShowHint     := True;
  Self.FView.btnNovo.Hint         := 'Adicionar um novo registro';
  Self.FView.btnEditar.ShowHint   := True;
  Self.FView.btnEditar.Hint       := 'Editar o registro corrente';
  Self.FView.btnGravar.ShowHint   := True;
  Self.FView.btnGravar.Hint       := 'Gravar o registro corrente';
  Self.FView.btnCancelar.ShowHint := True;
  Self.FView.btnCancelar.Hint     := 'Cnacelar a operação corrente';
  Self.FView.btnApagar.ShowHint   := True;
  Self.FView.btnApagar.Hint       := 'Apagar o registro corrente';
  Self.FView.btnFechar.ShowHint   := True;
  Self.FView.btnFechar.Hint       := 'Fechar';
  Self.FFactory.ControlControls(FView, TStatus.dsInactive);
  Self.FView.ShowModal;
end;

function TProdutoController.Delete(co_produto: Integer): Boolean;
begin
  Self.FStatus := TStatus.dsDelete;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
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

destructor TProdutoController.Destroy;
begin
  FreeAndNil(Self.FStrB);
  FreeAndNil(Self.FQuery);
  FreeAndNil(Self.FFactory);
  FreeAndNil(Self.FProdutoModel);
  FreeAndNil(Self.FView);
  inherited;
end;

procedure TProdutoController.edtValorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9', #8, ^V, ^C, ^X]) then
    Key := #0;
end;

procedure TProdutoController.edtValorKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Self.FFactory.FormatCurrecyValue(Sender, Key, Shift);
end;

procedure TProdutoController.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  Begin
    Key:= #0;
    Self.FView.Perform(Wm_NextDlgCtl,0,0);
  end;
end;

procedure TProdutoController.FormShow(Sender: TObject);
var
  LProdutos : TObjectList<TProdutoModel>;
  aIndex: Integer;
begin
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabConsulta;
  LProdutos := Self.GetAll;
  try
    if LProdutos.Count > 0 then
    begin
      Self.FView.FDMProduto.Open;
      Self.FView.FDMProduto.EmptyDataSet;
      for aIndex := 0 to Pred(LProdutos.Count) do
      begin
        Self.FView.FDMProduto.Insert;
        Self.FView.FDMProdutoCO_PRODUTO.AsInteger   := LProdutos[aIndex].co_produto;
        Self.FView.FDMProdutoTX_DESCRICAO.AsString  := LProdutos[aIndex].tx_descricao;
        Self.FView.FDMProdutoVL_VENDA.AsCurrency    := LProdutos[aIndex].vl_venda;
        Self.FView.FDMProdutoNU_ESTOQUE.AsFloat     := LProdutos[aIndex].nu_estoque;
        Self.FView.FDMProdutoDT_CADASTRO.AsDateTime := LProdutos[aIndex].dt_cadastro;

        if not Self.FFactory.DateTimeIsNull(LProdutos[aIndex].dt_alteracao) then
          Self.FView.FDMProdutoDT_ALTERACAO.AsDateTime  := LProdutos[aIndex].dt_alteracao;
        Self.FView.FDMProduto.Post;
      end;
      Self.FView.FDMProduto.First;
      Self.FStatus := TStatus.dsBrowse;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
    end else begin
      Self.FStatus := TStatus.dsInactive;
      Self.FFactory.ControlControls(Self.FView, Self.FStatus);
      Self.FView.FDMProduto.Close;
    end;
    Self.FFactory.EnableDisableControls(Self.FView, aDisable);
  finally
    FreeAndNil(LProdutos);
  end;
end;

function TProdutoController.GetAll: TObjectList<TProdutoModel>;
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

procedure TProdutoController.gridBaseDblClick(Sender: TObject);
begin
  if Self.FView.FDMProduto.RecordCount = 0 then
    Exit;
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FView.edtCodigo.Text          := FormatFloat('000000', Self.FView.FDMProdutoCO_PRODUTO.AsInteger);
  Self.FView.edtDescricao.Text       := Self.FView.FDMProdutoTX_DESCRICAO.AsString;
  Self.FView.edtValor.Text           := FormatFloat('#,##0.00', Self.FView.FDMProdutoVL_VENDA.AsCurrency);
  Self.FView.edtEstoque.Text         := FloatToStr(Self.FView.FDMProdutoNU_ESTOQUE.AsFloat);
  if Self.FStatus = TStatus.dsEdit then
    Exit;
  Self.FStatus                       := TStatus.dsBrowse;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
  Self.FFactory.EnableDisableControls(Self.FView, aDisable);
end;

procedure TProdutoController.Insert;
begin
  Self.FFactory.ClearControls(Self.FView);
  Self.FView.pgcPrincipal.ActivePage := Self.FView.tabCadastro;
  Self.FFactory.EnableDisableControls(Self.FView, aEnable);
  Self.FView.edtDescricao.SetFocus;
  Self.FView.edtCodigo.Text := FormatFloat('000000', Self.FFactory.GetNextID('INC_CO_PRODUTO', DMConexao.Conexao));
  Self.FStatus := TStatus.dsInsert;
  Self.FFactory.ControlControls(Self.FView, Self.FStatus);
end;

procedure TProdutoController.Save(aObject: TProdutoModel);
begin
  if Self.FStatus = TStatus.dsInsert then
  begin
    Self.FStrB.Clear;
    Self.FStrB.AppendLine('INSERT INTO PRODUTO (CO_PRODUTO, TX_DESCRICAO, VL_VENDA, NU_ESTOQUE)      ')
              .AppendLine('             VALUES (:CO_PRODUTO, :TX_DESCRICAO, :VL_VENDA, :NU_ESTOQUE); ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                             := Self.FStrB.ToString;
    Self.FQuery.ParamByName('CO_PRODUTO').AsInteger  := aObject.co_produto;
    Self.FQuery.ParamByName('TX_DESCRICAO').AsString := aObject.tx_descricao;
    Self.FQuery.ParamByName('VL_VENDA').AsCurrency   := aObject.vl_venda;
    Self.FQuery.ParamByName('NU_ESTOQUE').AsFloat    := aObject.nu_estoque;

    if not Self.FView.FDMProduto.Active then
      Self.FView.FDMProduto.Open;

    Self.FView.FDMProduto.Insert;
    Self.FView.FDMProdutoCO_PRODUTO.AsInteger  := aObject.co_produto;
    Self.FView.FDMProdutoTX_DESCRICAO.AsString := aObject.tx_descricao;
    Self.FView.FDMProdutoVL_VENDA.AsCurrency   := aObject.vl_venda;
    Self.FView.FDMProdutoNU_ESTOQUE.AsFloat    := aObject.nu_estoque;
    Self.FView.FDMProduto.Post;

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
    Self.FStrB.AppendLine('UPDATE PRODUTO                      ')
              .AppendLine('  SET TX_DESCRICAO = :TX_DESCRICAO, ')
              .AppendLine('      VL_VENDA     = :VL_VENDA,     ')
              .AppendLine('      NU_ESTOQUE   = :NU_ESTOQUE,   ')
              .AppendLine('      DT_ALTERACAO = :DT_ALTERACAO  ')
              .AppendLine('  WHERE CO_PRODUTO = :CO_PRODUTO;   ');

    Self.FQuery.SQL.Clear;
    Self.FQuery.SQL.Text                               := Self.FStrB.ToString;
    Self.FQuery.ParamByName('TX_DESCRICAO').AsString   := aObject.tx_descricao;
    Self.FQuery.ParamByName('VL_VENDA').AsCurrency     := aObject.vl_venda;
    Self.FQuery.ParamByName('NU_ESTOQUE').AsFloat      := aObject.nu_estoque;
    Self.FQuery.ParamByName('DT_ALTERACAO').AsDateTime := aObject.dt_alteracao;
    Self.FQuery.ParamByName('CO_PRODUTO').AsInteger    := aObject.co_produto;

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

end.

object DMConexao: TDMConexao
  OldCreateOrder = False
  Height = 335
  Width = 440
  object Conexao: TFDConnection
    Params.Strings = (
      'Database=C:\Projeto\TExcellent\db\DB25.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=127.0.0.1'
      'Port=3050'
      'DriverID=FB')
    TxOptions.AutoStop = False
    LoginPrompt = False
    Left = 72
    Top = 32
  end
  object FDFBDriverLink: TFDPhysFBDriverLink
    VendorLib = 'C:\Projeto\TExcellent\bin\fbclient.dll'
    Left = 152
    Top = 32
  end
  object FDGUIxErrorDialog1: TFDGUIxErrorDialog
    Provider = 'Forms'
    Left = 80
    Top = 120
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 200
    Top = 120
  end
end

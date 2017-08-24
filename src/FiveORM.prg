#include "Fivewin.ch" 
#include "xbrowse.ch" 
#include "metropnl.ch"
*
#DEFINE DE_ERRO_NOME "VALOR DO NOME DO CAMPO NÃO É CARACTERE"
#DEFINE DE_ERRO_TIPO "VALOR DO TIPO DO CAMPO NÃO É CARACTERE"
#DEFINE DE_ERRO_TAMANHO "VALOR DO TAMANHO DO CAMPO NÃO É NUMÉRICO"
#DEFINE DE_ERRO_DECIMAL "VALOR DO DECIMAL DO CAMPO NÃO É NUMÉRICO"
#DEFINE DE_ERRO_NOME_TAMANHO8 "NÚMERO DE CARACTERES É MAIOR QUE 8"
#DEFINE DE_ERRO_NOME_TAMANHO0 "NÚMERO DE CARACTERES É MENOR OU IGUAL A 0"
#DEFINE DE_ERRO_TAMANHO0 "TAMANHO NÃO PODE SER MENOR OU IGUAL A 0"
#DEFINE DE_ERRO_TAMANHO255 "TAMANHO NÃO PODE SER MAIOR QUE 255"
#DEFINE DE_ERRO_TAMANHO1 "TAMANHO NÃO PODE SER MAIOR QUE 1"
#DEFINE DE_ERRO_TAMANHO8 "TAMANHO NÃO PODE SER MAIOR QUE 8"
#DEFINE DE_ERRO_TAMANHO10 "TAMANHO NÃO PODE SER MAIOR QUE 10"
#DEFINE DE_ERRO_TIPOINVALIDO "TIPO DO CAMPO INVÁLIDO"
#DEFINE DE_ERRO_TAMANHODECIMAL0 "DECIMAL NÃO PODE SER MENOR QUE 0"
//----------------------------------------------------------------//
 
#include "fivewin.ch"
#include "dbinfo.ch"
#include "adodef.ch"
#include "xbrowse.ch"
#include "dbcombo.ch"
#include "Set.ch"

#define COLOR_BTNFACE       15

#ifdef __XHARBOUR__
   #xtranslate HB_STRTOHEX( <c> ) => STRTOHEX( <c> )
   #xtranslate HB_TTOS( <t> )     => TTOS( <t> )
   #xtranslate HB_CTOT( <c> )     => CTOT( <c> )
   #xtranslate HB_DateTime()      => DateTime()
#endif

#define FLD_NAME  1
#define FLD_VAL   2
#define FLD_RW    3
#define FLD_PIC   4
#define FLD_TYP   4
#define FLD_VLD   5
#define FLD_DFLT  6
#define FLD_HIDE  7
#define FLD_SIZE  FLD_HIDE

//----------------------------------------------------------------------------//

static lExit := .f.

//----------------------------------------------------------------------------//

CLASS Tabela

   DATA oMae
   DATA oModelo
   DATA aAtributo AS ARRAY INIT {}
   DATA aCampo AS ARRAY INIT {}
   DATA aIndice AS ARRAY INIT {}
   DATA aTabelasFilhas AS ARRAY INIT {}
   DATA cAlias AS CHARACTER INIT ""
   DATA lExclusive AS LOGICAL INIT .F.
   
	Method New(oModelo) CONSTRUCTOR
   Method InitCampos(oModelo)
   Method InitIndice()
   Method AddIndice(oIndice)
   Method PegaConexao()  
	Method SQL(cDeclaracao)
	Method ManterTabela() 
   Method ManterIndice(lModifica) 
   Method Usar(lExclusive)
   Method Fechar() 
   Method End() 
   Method UsarIndex() 
   Method Estrutura()
   Method EstruturaModelo()
   Method Ordenar(cTag)
   Method Procurar(uValue)
   Method Localizar(uValue)
   Method Travar()
   Method Filtrar(uValue)
   Method Manter(lIncluir,oEntidade,nID)
   Method ValidaEntidade(oEntidade)
   Method Incluir(oEntidade)
   Method PegaTabelaFilhaPorModelo(oEntidade)
   Method Alterar(oEntidade,nID)
   Method Remover(nID)
   Method PegarModel(nID)
   Method PegarArray()
   Method PegarAlias()
   Method EstruturaIndiceModelo()     
   Method EstruturaIndice()
	Method LimparEmBrancos()   
	Method CamposToArray()
	Method AdicionarIndice(cKey,cTag)
	Method PegaCampoPorAtributo(cAtributo)       
	Method PegaAtributoPorCampo(oCampo) 
	DESTRUCTOR End()
ENDCLASS

METHOD New(oModelo,oMae) CLASS Tabela
	*
	Local cRdd:= RddSetDefault()
	Default oMae:= NIL
		
	IF cRdd # "SQLRDD"
	   Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("RDD:" + cRdd + " NÃO É SUPORTADO") ))
	ENDIF
	*
	try
		SR_CheckConnection() // Já executa um RUNTIME ERRO
	catch
	   MsgStop("Não existe conexão com o servidor do banco de dados!","Ocorreu um problema")
	   QUIT
	end	
	*
	if oModelo == nil
	   Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("oModelo é nil") ))
	endif
	*
	IF !oModelo:IsKindOF( "ENTIDADE" )
	   Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("oModelo não é uma Entidade") ))
	ENDIF
	*
	::oModelo := oModelo
	::oMae := oMae
	
	IF ::oMae # NIL
	   IF !oMae:IsKindOF( "ENTIDADE" )
	      Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("oMae não é uma Entidade") ))
	   ENDIF
	ENDIF
	*
	::InitCampos()
	::InitIndice()
	*
	// ::oModelo:= &(::oModelo:ClassName)():NewModel()
	*
Return Self
*
Method InitCampos() Class Tabela
	Local aDados := aOData(::oModelo)
	Local oCampo:=NIL,cName:="",uData:=NIL,cType:="",nLen:=0,nDec:=0
	Local lIsNotObject:=.t. 
	Local oTabelaRelacionamento:=NIL,nItem := 1 
	*
	For nItem := 1 to len(aDados)
	   DEBUG:=.F.
		oTabelaRelacionamento:=NIL
		lIsNotObject:=.t.
		oCampo:=NIL
		cName:=""
	   uData:=NIL
	   cType:=""
	   nLen:=0
	   nDec:=0
	   *
	   cName:=aDados[nItem]
	   uData:= ::oModelo:&(aDados[nItem])
	   cType := Valtype(uData)
		*
	   Do Case
	   case cType == "C"
	      nLen := len(cValToChar(uData))
	      if nLen > 255
		      cType := "M"
		      nLen := 10
			endif
		case cType == "D"
	      nLen := 8
	   case cType == "N"
	      nLen := len(cValToChar(uData))
	      nAt := AT(".",cValToChar(uData))
	      if nAt > 0
	         nDec:= nLen - nAt
	      endif
	      nLen := len(strtran(cValToChar(uData),"."))
	   case cType == "L"
	      nLen := 1
	   case cType == "M"
	      nLen := 10
	   case cType == "A" // One-To-Many
	      if len(uData) > 0
				if Valtype(uData[1]) == "O"
	            if uData[1]:IsKindOF( "ENTIDADE" )
	               oTabelaRelacionamento := Tabela():New(uData[1],::oModelo) // Cria tabela filha
	               AADD(::aAtributo,Atributo():New(cName,NIL))
						aadd(::aTabelasFilhas,oTabelaRelacionamento)
	            endif
	         endif
	      endif
	      lIsNotObject:=.f.
	   case cType == "O" // One-To-One
	      IF uData:IsKindOF( "ENTIDADE" )
				oCampo := Campo():New("NID_"+uData:ClassName,"N",20,0)
	         oTabelaRelacionamento := Tabela():New(uData) // Cria tabela filha
	         aadd(::aTabelasFilhas,oTabelaRelacionamento)
	         ::AddIndice(Indice():New(uData:ClassName+"FK",oCampo:cNome,::oModelo:ClassName))
	      ENDIF
	      lIsNotObject:=.f.
	   ENDCASE
	   *
		if lIsNotObject
	      oCampo := Campo():New(cName,cType,nLen,nDec)
	   endif
	   
		IF oCampo # nil
			AADD(::aAtributo,Atributo():New(cName,oCampo))
			AADD(::aCampo,oCampo)
	   ENDIF
	Next
	
	*	
	IF ::oMae # NIL
	   oCampo := Campo():New("NID_"+::oMae:ClassName,"N",20,0)
	   AADD(::aAtributo,Atributo():New("NID_"+::oMae:ClassName,NIL))
		AADD(::aCampo,oCampo)
	   ::AddIndice(Indice():New(::oMae:ClassName+"FK",oCampo:cNome,::oModelo:ClassName))
	ENDIF
	*		
Return

Method InitIndice() Class Tabela
	Local oCampo:=NIL
				
	IF LEN(::aCampo) > 0
	   For Each oCampo IN ::aCampo
	      if upper(oCampo:cNome) == "NID"
	         ::AddIndice(Indice():New(::oModelo:ClassName+"PK",oCampo:cNome,::oModelo:ClassName,.T.))
	      endif
	      if upper(oCampo:cNome) == "DDATAREG"
	         ::AddIndice(Indice():New(::oModelo:ClassName+"DTREG",oCampo:cNome,::oModelo:ClassName,.T.))
	      endif
	   Next
	ENDIF
	
Method CamposToArray() Class Tabela
		 local aEstrutura:={},oCampo
		 For each oCampo in ::aCampo
		     aadd(aEstrutura,oCampo:ToArray())
		 next
		 return aEstrutura
Method AddIndice(oIndice) Class Tabela
	aadd(::EstruturaIndiceModelo(),oIndice)

Method AdicionarIndice(cKey,cTag) Class Tabela
	::AddIndice(Indice():New(cTag,cKey,::oModelo:ClassName))
	

Method ManterTabela()  Class Tabela// Criar ou Alterar Tabela
	Local aEstruturaAtual:={},oTabelaFilha:=NIL
	*
	if !SR_ExistTable(::oModelo:ClassName)
		DbCreate(::oModelo:ClassName,::CamposToArray())
	else
	   ::Usar(.f.,.t.)
	   aEstruturaAtual:= ::Estrutura()
	   if Verifica_Estrutura(::aCampo,aEstruturaAtual)
	      SR_ChangeStruct(::oModelo:ClassName,::CamposToArray())
	      SR_DropIndex(::oModelo:ClassName)
	   endif
	   ::Fechar()
	endif
	*
	For each oTabelaFilha in ::aTabelasFilhas
		IF oTabelaFilha:oModelo:IsKindOF( "ENTIDADE" )
	      oTabelaFilha:ManterTabela()
	   ENDIF
	Next
	*
Method PegaConexao()  Class Tabela // Criar Ou Alterar Indice
Return SR_GetCnn()
Method SQL(cDeclaracao)  Class Tabela // Criar Ou Alterar Indice
      Local aReturn:={}
      ::PegaConexao():exec( cDeclaracao,.t.,.t.,@aReturn )
		if Len( aReturn ) == 1
         aReturn  := aReturn[ 1 ]
         if Len( aReturn ) == 1
            aReturn  := aReturn[ 1 ]
         endif
      endif
      return aReturn

Method ManterIndice(lModifica)  Class Tabela // Criar Ou Alterar Indice
	Local oTabelaFilha:=NIL,oIndice:=NIL,aEstruturaAtual:={} , bKey := NIL
	DEFAULT lModifica:=.F.
	*
	IF lModifica
		if SR_ExistTable(::oModelo:ClassName)
			SR_DropIndex(::oModelo:ClassName)
			TRY
			::SQL("ALTER TABLE "+::oModelo:ClassName+" DROP INDEX nid")
			CATCH oError
				lMkDir(".\LOG\")
				MemoWrit(".\LOG\NUTABELA.LOG",oError:Description+CRLF+MemoRead(".\LOG\NUTABELA.LOG"))
			END
		endif	
	ENDIF	
	if SR_ExistTable(::oModelo:ClassName) 
		if !SR_ExistIndex(::oModelo:ClassName)
		   ::Usar(.f.,.t.)
			::LimparEmBrancos()	
			For each oIndice in ::EstruturaIndiceModelo()
				 Select(::cAlias)
				 (::cAlias)->(OrdCreate(oIndice:cFile,oIndice:cTag,oIndice:cKey,FWGENBLOCK(oIndice:cKey)))
				 IF oIndice:lUnique
				 	 *::SQL("ALTER TABLE "+::oModelo:ClassName+" ADD UNIQUE INDEX nid (nid)")
				 ENDIF 
		 	Next
			TRY
			::SQL("ALTER TABLE "+::oModelo:ClassName+" ADD UNIQUE INDEX nid (nid)")
			CATCH oError
				lMkDir(".\LOG\")
				cErro:=""
				if File(".\LOG\NUTABELA.LOG")
					cErro:=MemoRead(".\LOG\NUTABELA.LOG")
				endif	
				MemoWrit(".\LOG\NUTABELA.LOG",oError:Description+CRLF+cErro)
			END
		   ::Fechar()
		endif
	endif
	*
	For Each oTabelaFilha in ::aTabelasFilhas
	   IF oTabelaFilha:oModelo:IsKindOF( "ENTIDADE" )
	      oTabelaFilha:ManterIndice()
	   ENDIF
	Next
	*

Method EstruturaIndiceModelo() Class Tabela
		 return ::aIndice

Method EstruturaIndice() Class Tabela
		 Local aEstruturaIndice:={},nrDeIndices:=0,nrDeIndice:=0,oIndice
		 *
		 if !Empty(::cAlias)
		 	 nrDeIndices:= OrdCount(::oModelo:ClassName)
          for nrDeIndice:=1 to nrDeIndices
			 	  oIndice:= Indice():New(OrdName( nrDeIndice ), OrdKey ( nrDeIndice ), OrdBagName( nrDeIndice ), OrdIsUnique( nrDeIndice ), OrdDescend( nrDeIndice ))
			 	  aadd(aEstruturaIndice,oIndice)
			 next
		 endif
		 *
		 return aEstruturaIndice

Method LimparEmBrancos()  Class Tabela
		 IF Select(::cAlias) > 0
		 	 (::cAlias)->(DbPack())
		 	 return .t.
		 ENDIF
		 return .f.	 
Method Estrutura()  Class Tabela
		 IF Select(::cAlias) > 0
			 RETURN (::cAlias)->(DbStruct())
	    ELSE	
		 	 RETURN {}
	    ENDIF

Method EstruturaModelo()  Class Tabela

RETURN ::aCampo

Method Ordenar(cTag,uScopoInicial,uScopoFinal)  Class Tabela
		 DEFAULT uScopoInicial:=NIL,uScopoFinal:=NIL
		 IF Valtype(cTag) # "C"
		 	 Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("cTag não é CARACTERE") ))
		 ENDIF
		 IF Select(::cAlias) > 0
		    (::cAlias)->(DbSetOrder( cTag ))
		    IF uScopoInicial # NIL .AND. uScopoFinal # NIL
				 (::cAlias)->(OrdScope(0,uScopoInicial))
			    (::cAlias)->(OrdScope(1,uScopoFinal))
		    ENDIF
			 (::cAlias)->(DbGoTop())
			 return .t.
		 ENDIF
		 return .f.   
RETURN NIL

Method Procurar(uValue,lSemelhante,lUltimo)  Class Tabela
		 DEFAULT lSemelhante:=.F.,lUltimo:=.F.
		 IF uValue == nil
		 	 Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("uValue é NIL") ))
		 ENDIF
		 IF Valtype(lSemelhante) # "L"
		 	 Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("lSemelhante não é logico") ))
		 ENDIF
		 IF Valtype(lUltimo) # "L"
		 	 Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("lUltimo não é logico") ))
		 ENDIF
		 *
		 IF Select(::cAlias) > 0
			 return (::cAlias)->(DbSeek(uValue,lSemelhante,lUltimo ))
		 ENDIF
		 return .f.   

RETURN NIL

Method Travar()  Class Tabela
	    IF Select(::cAlias) > 0
	       RETURN (::cAlias)->(RLOCK())
	    ENDIF
     	 RETURN .F.
		    
Method Localizar(uValue)  Class Tabela
		 //TOTEST
		 IF Select(::cAlias) > 0
		 	 (::cAlias)->(__dbLocate(FWGENBLOCK(OrdName(OrdNumber())+" = "+cValToChar(uValue))))
			 RETURN (::cAlias)->(Found())
		 ELSE
		 	 RETURN .F.		 
		 ENDIF	 
		 *	
Method Filtrar(uValue)  Class Tabela
       IF Select(::cAlias) > 0
		 	 SR_SetFilter(uValue)
		 	 (::cAlias)->(DbGoTop())
		 	 RETURN .T.
		 ENDIF	 
		 RETURN .F.
		 *

Method Manter(lIncluir,oEntidade,nID)  Class Tabela
		Local oAtributo:=NIL, oCampo:=NIL, nPosicaoCampo:=0, cAtributo:="", cValType:="", uData:=NIL, oFilho := NIL
		Local oTabelaFilha:=NIL, oOneToOne:= 0, oOneToMany:=NIL, oFilhos := NIL, tEntidade := NIL
		*
		::Usar(.t.)
		*
		IF lIncluir
			(::cAlias)->(DbAppend())
			IF !::Travar()
			   return 0
			ENDIF
			(::cAlias)->(DbUnlock())
			(::cAlias)->(DbCommit())
		ELSE
			::Ordenar(::oModelo:ClassName+"PK")
			IF !::Procurar(nID)
				return 0
			ENDIF	
		ENDIF	
		*
		IF !::Travar()
		   return 0
		ENDIF
		*
		*SysWait(1)
		(::cAlias)->NID := (::cAlias)->(RecNo())
		(::cAlias)->DDATAREG := DATE()
		IF ::oMae # NIL
			(::cAlias)->&("NID_"+::oMae:ClassName) := ::oMae:nID
		ENDIF
		*
		oEntidade:nId := (::cAlias)->NID
		oEntidade:dDataReg := (::cAlias)->DDATAREG
		*
		for each oAtributo in ::aAtributo
			oCampo:=oAtributo:oCampo
			cAtributo:=oAtributo:cAtributo
			IF LEFT(upper(cAtributo),4) == upper("NID_")
				cAtributo:=""
			ENDIF
			if !Empty(cAtributo)
		      uData := oEntidade:&(cAtributo)                
		      cValType:= Valtype(uData)
				*
				DO CASE
		      CASE cValType == "C" .or. cValType == "D" .or. cValType == "N" .or. cValType == "L" .or. cValType == "M" // Padrão
					if upper(cAtributo) # "NID" .or. upper(cAtributo) # "DDATAREG" 
		            nPosicaoCampo := (::cAlias)->(FieldPos(oCampo:cNome))
		            (::cAlias)->(FieldPut(nPosicaoCampo,oEntidade:&(oCampo:cNome)))
		         endif
		      CASE cValType == "A" // One-To-Many
					if !lIncluir
				      * Inicializar Classe Modelo
				      * Pegar Filhos
				      * Limpar
				      tEntidade := &(oEntidade:ClassName)():New()
						oOneToMany    := tEntidade:&(cAtributo)
						*
						if len(oOneToMany) > 0
						   oOneToMany:=oOneToMany[1]
						   if Valtype(oOneToMany) == "O"
						      if oOneToMany:IsKindOF( "ENTIDADE" )
						         oTabelaFilha := ::PegaTabelaFilhaPorModelo(oOneToMany)
						         oTabelaFilha:oMae := oEntidade
						         oFilhos      := oTabelaFilha:PegarModel(oEntidade:nID)
						         if len(oFilhos) > 0
						            if Valtype(oFilhos) == "A"
						               for each oFilho in oFilhos
						                  if oFilho:IsKindOF( "ENTIDADE" )
						                     oTabelaFilha:oMae := NIL
						                     oTabelaFilha:Remover(oFilho:nID)
						                  endif
						               next
						            endif
						         endif
						      endif
						   endif
						endif
			         *
					endif	
					for each oFilho in uData
						 if oFilho:IsKindOF( "ENTIDADE" )
					    	 oTabelaFilha := ::PegaTabelaFilhaPorModelo(oFilho)
					    	 oTabelaFilha:oMae := oEntidade
							 oTabelaFilha:Incluir(oFilho)
		             endif
		         next
		      CASE cValType == "O" // One-To-One -> Grava o ID do Model existente
					IF uData:IsKindOF( "ENTIDADE" )
				    	 oTabelaFilha := ::PegaTabelaFilhaPorModelo(uData)
						 oOneToOne := oTabelaFilha:PegarModel(uData:nID)
			          if oOneToOne # NIL
			             IF oOneToOne:IsKindOF( "ENTIDADE" )
						       (::cAlias)->&("NID_"+uData:ClassName) := oOneToOne:nID
						    ENDIF   
						 ENDIF   
		         ENDIF
		      ENDCASE
		   endif
		next
		(::cAlias)->(DbUnlock())
		(::cAlias)->(DbCommit())
		*
RETURN oEntidade:nId
Method ValidaEntidade(oEntidade) Class Tabela
		 IF !oEntidade:IsKindOF( ::oModelo:ClassName )
		   Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("oEntidade não é do mesmo tipo do Controle:"+::oModelo:ClassName) ))
		 ENDIF

Method Incluir(oEntidade) Class Tabela
		::ValidaEntidade(oEntidade)
		RETURN ::Manter(.t.,oEntidade)
Method PegaTabelaFilhaPorModelo(oFilho) Class Tabela
		 Local oTabelaFilha:=NIL
		 For each oTabelaFilha in ::aTabelasFilhas
			  IF oTabelaFilha:oModelo:IsKindOF( oFilho:ClassName )
           	  RETURN oTabelaFilha
           ENDIF		
       Next
       RETURN oTabelaFilha

Method PegaCampoPorAtributo(cAtributo)  Class Tabela
		 Local oAtributo,oCampo:=NIL
		 for each oAtributo in ::aAtributo
				if oAtributo:cAtributo == cAtributo
		 	     return oCampo
		 	  endif
		 next		  
		 return oCampo

Method PegaAtributoPorCampo(oCampo)  Class Tabela
		 Local oAtributo,cAtributo:=""
		 for each oAtributo in ::aAtributo
				if oAtributo:oCampo:cNome == oCampo:cNome
		 	     return oAtributo:cAtributo
		 	  endif
		 next		  
		 return cAtributo

Method Alterar(oEntidade,nID)  Class Tabela
		 ::ValidaEntidade(oEntidade)
		 DEFAULT nID:=oEntidade:nID
		 IF Valtype(nID) # "N"
		 	 Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("nID não é Numérico") ))
		 ENDIF
		 IF nID == 0
		    MsgStop("ID é igual a zero não é possivel alterar!","Problema")
		    return 0
		 ENDIF
		 RETURN ::Manter(.F.,oEntidade,nID)

Method Remover(nID) Class Tabela
		Local oAtributo:=NIL,oCampo:=NIL,nPosicaoCampo:=0,cAtributo:="",cValType:="",uData:=NIL,oFilho := NIL,oTabelaFilha:=NIL,nIDTemUm:= 0 , oEntidade:=NIL
		*
		::Usar(.t.)
		::Ordenar(::oModelo:ClassName+"PK")
		IF !::Procurar(nID)
			return .F.
		ENDIF	
		*
		IF !::Travar()
		   return .F.
		ENDIF
		*
		oEntidade:= ::PegarModel(nID)
		*
		for each oAtributo in ::aAtributo
			oCampo:=oAtributo:oCampo
			cAtributo:=oAtributo:cAtributo
			IF LEFT(upper(cAtributo),4) == upper("NID_")
				cAtributo:=""
			ENDIF
		   if !Empty(cAtributo)
		      uData := oEntidade:&(cAtributo)                
		      cValType:= Valtype(uData)
				*
				DO CASE
		      CASE cValType == "A" // One-To-Many a ligacao one-to-many remove pois é tratada como mestre detalhe
					for each oFilho in uData
					    if oFilho:IsKindOF( "ENTIDADE" )
					    	 oTabelaFilha := ::PegaTabelaFilhaPorModelo(oFilho)
					    	 oTabelaFilha:oMae := NIL
						 	 oTabelaFilha:Remover(oFilho:nID)
		             endif
		         next
		      CASE cValType == "O" // One-To-One A ligacao one-to-one nao remove pois pode ser usado para outros cadastros
					IF uData:IsKindOF( "ENTIDADE" )
				    	 *oTabelaFilha := ::PegaTabelaFilhaPorModelo(uData)
					    *oTabelaFilha:Remover(uData:nID)
		         ENDIF
		      ENDCASE
		   endif
		next
		*
		(::cAlias)->(DbDelete())
		*
		(::cAlias)->(DbUnlock())
		(::cAlias)->(DbCommit())
RETURN .t.

Method PegarModel(nID,lDebug) Class Tabela
		Local oEntidade := nil,aFilhos:={},oAtributo:=NIL,oCampo:=NIL,cAtributo:="",uData:=NIL,cValType:="" ,oFilho:=NIL,oTabelaFilha:=NIL
		Default lDebug:=.f.
		::Usar(.t.)
		IF ::oMae # NIL
		   ::Ordenar(::oMae:ClassName+"FK")
		ELSE
		   ::Ordenar(::oModelo:ClassName+"PK")
		ENDIF   
		IF !::Procurar(nID)
			return &(::oModelo:ClassName)():NewModel()
		ENDIF	
		*
		While .t.
				oEntidade := &(::oModelo:ClassName)():New()
				for each oAtributo in ::aAtributo
					oCampo:=oAtributo:oCampo
					cAtributo:=oAtributo:cAtributo
					IF LEFT(upper(cAtributo),4) == upper("NID_")
						cAtributo:=""
					ENDIF
				   if !Empty(cAtributo)
				      uData := oEntidade:&(cAtributo)                
				      cValType:= Valtype(uData)
						*
						DO CASE
				      CASE cValType == "C" .or. cValType == "D" .or. cValType == "N" .or. cValType == "L" .or. cValType == "M" // Padrão
							   oEntidade:&(cAtributo) := (::cAlias)->(FieldGet(FieldPos(oCampo:cNome)))
				      CASE cValType == "A" // One-To-Many
							for each oFilho in oEntidade:&(cAtributo)
							    if oFilho:IsKindOF( "ENTIDADE" )
								    oTabelaFilha := ::PegaTabelaFilhaPorModelo(oFilho)
							    	 oTabelaFilha:oMae := oEntidade
								    oEntidade:&(cAtributo) := oTabelaFilha:PegarModel(nID)
								 	 EXIT
				             endif
				         next
				      CASE cValType == "O" // One-To-One
							IF uData:IsKindOF( "ENTIDADE" )
						    	oTabelaFilha := ::PegaTabelaFilhaPorModelo(uData)
							   oEntidade:&(cAtributo) := oTabelaFilha:PegarModel((::cAlias)->&("NID_"+uData:ClassName))
				         ENDIF
				      ENDCASE
				   endif
				next
				IF ::oMae # NIL
				   AADD(aFilhos,oEntidade)
					(::cAlias)->(DbSkip())
				   IF (nID #  (::cAlias)->&("NID_"+::oMae:ClassName)) .or. (::cAlias)->(Eof())
				      EXIT
				   ENDIF
				ELSE
					exit
				ENDIF
		end

		IF ::oMae # NIL
			RETURN aFilhos
		ELSE	
			RETURN oEntidade
		ENDIF	

Method PegarArray() Class Tabela // Retornar objetos da tabela corrente em um array
       Local aRegistros := {},oEntidade:=NIL
		 ::Usar(.t.)
		 ::Ordenar(::oModelo:ClassName+"PK")
       While !((::cAlias)->(Eof()))
             oEntidade := ::PegarModel((::cAlias)->nID)
				 IF oEntidade # NIL
				 	 aadd(aRegistros,oEntidade)
				 ENDIF	 
				 (::cAlias)->(DbSkip())
		 End
		 (::cAlias)->(DbGoTop())
		 RETURN aRegistros
RETURN NIL

Method PegarAlias() Class Tabela // Retornar objetos filhos one-to-many
		IF Select(::cAlias) > 0       
			RETURN (::cAlias)->(Alias())
		ENDIF
		RETURN ""	

Method Usar(lNewArea,lExclusive)  Class Tabela // Criar Ou Alterar Indice
	Default lExclusive:=.f.,lNewArea:=.t.
		    
	IF Valtype(lExclusive) # "L"
	   Eval( ErrorBlock(), _FWGenError( 6, AnsiToOem("lExclusive não é Logico") ))
	ENDIF
	*
	IF !Empty(::cAlias)
	   IF lExclusive # ::lExclusive
	      ::Fechar()
	   ENDIF
	ENDIF
	*
	IF Select(IIF(!Empty(::cAlias),::cAlias,::oModelo:ClassName)) == 0
	   DbUseArea(lNewArea,"SQLRDD",::oModelo:ClassName,cGetNewAlias(::oModelo:ClassName),!lExclusive)
	   IF NetErr()
	      MsgAlert("Tabela aberta em modo exclusivo!","Tabela:"+::oModelo:ClassName)
	      ::Fechar()
	      RETURN .F.
	   ENDIF
	   ::lExclusive := lExclusive
	   ::cAlias := Alias()
		*
		::UsarIndex()
	   *
	ENDIF
	*
RETURN .T.

Method End() Class Tabela
		::Fechar()
Method Fechar()  Class Tabela
	Local oTabelaFilha:=NIL
	IF Select(::cAlias) > 0
	   (::cAlias)->(DbCloseArea())
		For each oTabelaFilha in ::aTabelasFilhas
			IF oTabelaFilha:oModelo:IsKindOF( "ENTIDADE" )
		      oTabelaFilha:Fechar()
		   ENDIF
		Next
	   Return .t.
	ENDIF
Return .f.

Method UsarIndex()  Class Tabela
	IF Select(::cAlias) > 0
		IF SR_ExistIndex(::oModelo:ClassName)
		   (::cAlias)->(DbSetIndex())
		   RETURN .T.
		ENDIF
	ENDIF
	RETURN .F.	
//----------------------------------------------------------------//

CLASS Campo

   DATA cNome AS CHARACTER INIT SPACE(8)
   DATA cTipo AS CHARACTER INIT SPACE(1)
   DATA nTamanho AS NUMERIC INIT 0
   DATA nDecimal AS NUMERIC INIT 0
   
   
   METHOD New() CONSTRUCTOR
   Method toString()
   Method ToArray()
ENDCLASS


METHOD New(cNome,cTipo,nTamanho,nDecimal) CLASS Campo
	
::cNome   := cNome
::cTipo   := cTipo
::nTamanho:= nTamanho
::nDecimal:= nDecimal
*
IF Valtype(cNome) == "C"
   IF LEN(cNome) > 8
      *Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_NOME_TAMANHO8 ) )
   ENDIF
   IF LEN(cNome) <= 0
      Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_NOME_TAMANHO0 ) )
   ENDIF
ELSE
   Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_NOME ) )
ENDIF
*
IF Valtype(cTipo) == "C"
   IF Valtype(nTamanho) == "N"
      IF nTamanho <= 0
         Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO0 ) )
      ELSE
         DO CASE
         CASE cTipo == "C"
            IF nTamanho > 255
               Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO255 ) )
            ENDIF
         CASE cTipo == "N"
            IF nTamanho > 255
               Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO255 ) )
            ENDIF
         CASE cTipo == "L"
            IF nTamanho > 1
               Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO1 ) )
            ENDIF
         CASE cTipo == "D"
            IF nTamanho > 8
               Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO8 ) )
            ENDIF
         CASE cTipo == "M"
            IF nTamanho > 10
               Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO10 ) )
            ENDIF
         OTHERWISE
            Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TIPOINVALIDO ) )
         ENDCASE
      ENDIF
   ELSE
      Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHO ) )
   ENDIF
ELSE
   Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TIPO ) )
ENDIF
*
IF Valtype(nDecimal) == "N"
   IF nDecimal < 0
      Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_TAMANHODECIMAL0 ) )
   ENDIF
ELSE
   Eval( ErrorBlock(), _FWGenError( 6, ::toString()+":"+DE_ERRO_DECIMAL ) )
ENDIF
	
return Self
Method ToArray() Class Campo
		 return {::cNome,::cTipo,::nTamanho,::nDecimal}
Method toString() Class Campo
return "(cNome:"+cValToChar(::cNome)+","+"cTipo:"+cValToChar(::cTipo)+","+"nTamanho:"+cValToChar(::nTamanho)+","+"nDecimal:"+cValToChar(::nDecimal)+")"

//----------------------------------------------------------------//

CLASS Atributo
   DATA cAtributo, oCampo
   
   METHOD New(cAtributo, oCampo) CONSTRUCTOR

ENDCLASS

METHOD New(cAtributo, oCampo) CLASS Atributo
		::cAtributo := cAtributo
		::oCampo    := oCampo

return Self
//----------------------------------------------------------------//

CLASS Indice
   DATA cFile, cTag, cKey, lUnique, lDescend
   
   METHOD New(cTag, cKey, cFile,lUnique, lDescend) CONSTRUCTOR

ENDCLASS

METHOD New(cTag, cKey, cFile, lUnique, lDescend) CLASS Indice
Default lUnique:=.f.,lDescend:=.f.
*
IF cFile == nil
   Eval( ErrorBlock(), _FWGenError( 6, "cFile não pode ser NIL" ) )
ELSE
   IF Valtype(cFile) # "C"
      Eval( ErrorBlock(), _FWGenError( 6, "O Tipo de cFile não é caractere" ) )
   ENDIF
ENDIF
*
IF cTag == nil
   Eval( ErrorBlock(), _FWGenError( 6, "cTag não pode ser NIL" ) )
ELSE
   IF Valtype(cTag) # "C"
      Eval( ErrorBlock(), _FWGenError( 6, "O Tipo de cTag não é caractere" ) )
   ENDIF
ENDIF
*
IF cKey == nil
   Eval( ErrorBlock(), _FWGenError( 6, "cKey não pode ser NIL" ) )
ELSE
   IF Valtype(cKey) # "C"
      Eval( ErrorBlock(), _FWGenError( 6, "O Tipo de cKey não é caractere" ) )
   ENDIF
ENDIF
*
if Empty(cFile)
   Eval( ErrorBlock(), _FWGenError( 6, "cFile está vazio" ) )
endif
*
if Empty(cTag)
   Eval( ErrorBlock(), _FWGenError( 6, "cTag está vazio" ) )
endif
*
if Empty(cKey)
   Eval( ErrorBlock(), _FWGenError( 6, "cKey está vazio" ) )
endif
*
::cFile  := cFile
::cTag   := cTag
::cKey   := cKey
::lUnique:= lUnique
::lDescend:= lDescend               
*                                   
Return Self

//----------------------------------------------------------------//

CLASS Entidade

   DATA nId AS NUMERIC INIT Number(20,0)
   DATA dDataReg AS DATE init EmptyDate()
   METHOD New() CONSTRUCTOR
   METHOD NewModel() CONSTRUCTOR

ENDCLASS

METHOD NewModel() CLASS Entidade
 		 Local aDados:=aOData(Self),nItem:=1,uData:=NIL,cType:=""
 		 *
 		 For nItem:=1 TO len(aDados)
				uData:= SELF:&(aDados[nItem])
			   cType := Valtype(uData)
			   DO CASE
			   CASE cType == "C" .or. cType == "D" .or. cType == "N" .or. cType == "L" .or. cType == "M"
			   	  SELF:&(aDados[nItem]) := uValBlank(uData)
			   CASE cType == "A" // One-To-Many
			      if len(uData) > 0
						if Valtype(uData[1]) == "O"
			            if uData[1]:IsKindOF( "ENTIDADE" )
			            	*Self:&(aDados[nItem]) := { &(uData[1]:ClassName)():NewModel() }
			            	Self:&(aDados[nItem]) := { }
			            endif
			         endif
			      endif
			   CASE cType == "O" // One-To-One
			      IF uData:IsKindOF( "ENTIDADE" )
			      	Self:&(aDados[nItem]) := NIL 
			      ENDIF
			   ENDCASE
		 Next
 		 *
return Self

METHOD New() CLASS Entidade
		
return Self
		

//----------------------------------------------------------------//
Static Function Verifica_Estrutura(aEstCorreta,aEstAtual)
   && Pode Definição a aEstCorreta é a que escolhemos para ser a correta
   Local x := 0
   Local tam1 := len(aEstCorreta) , tam2:=len(aEstAtual)
   if tam1 <> tam2
      return .t.
   endif
   for x:=1 to tam1
      Pos := ASCAN( aEstAtual , {|aField| aField[1] == aEstCorreta[x][1] },1,len(aEstAtual) )
      if Pos > 0
         if upper(aEstCorreta[x][1]) <> upper(aEstAtual[Pos][1])
            return .t.
         endif
         if upper(aEstCorreta[x][2]) <> upper(aEstAtual[Pos][2])
            return .t.
         endif
         if aEstCorreta[x][3] <> aEstAtual[Pos][3]
            return .t.
         endif
         if aEstCorreta[x][4] <> aEstAtual[Pos][4]
            return .t.
         endif
      else
         return .f.
      endif
   next
   return .f.

Static Function Verifica_Estrutura_Indice(aEstCorreta,aEstAtual)
   && Pode Definição a aEstCorreta é a que escolhemos para ser a correta
   Local x := 0
   Local tam1 := len(aEstCorreta) , tam2:=len(aEstAtual)
   if tam1 <> tam2
      return .t.
   endif
   for x:=1 to tam1
      Pos := ASCAN( aEstAtual , {|aField| aField:cTag == aEstCorreta[x]:cTag },1,len(aEstAtual) )
      if Pos > 0
         if upper(aEstCorreta[x]:cTag) <> upper(aEstAtual[Pos]:cTag)
            return .t.
         endif
         if upper(aEstCorreta[x]:cKey) <> upper(aEstAtual[Pos]:cKey)
            return .t.
         endif
         if aEstCorreta[x]:lUnique <> aEstAtual[Pos]:lUnique
            *return .t.
         endif
         if aEstCorreta[x]:lDescend <> aEstAtual[Pos]:lDescend
            return .t.
         endif
      else
         return .f.
      endif
   next
   return .f.
   //----------------------------------------------------------------//
Function EmptyDate()
		   RETURN CTOD("  /  /    ")
Function Number(nLen,nDec)
   return val(replicate("9",nLen-nDec)+"."+replicate("9",nDec))
	
Function ConnectCONFIGMySQL(lChkDatabase,oDlgRun)
   LOCAL n        := 0

   Default lChkDatabase := .f.
      
   REQUEST SQLRDD
   REQUEST SR_MYSQL
   REQUEST DBFCDX

   fileMysql := MemoRead("SQL.CONFIG")
   aDadosMysql:=HB_ATokens(fileMysql,CRLF)
   if len(aDadosMysql) > 5
      cIp:=desprotege(aDadosMysql[1],30)
      cUser:=desprotege(aDadosMysql[2],30)
      cPassword:=desprotege(aDadosMysql[3],30)
      cDatabase:=desprotege(aDadosMysql[4],30)
      cPort:=desprotege(aDadosMysql[5],30)
   else
      FErase("SQL.CONFIG")
   endif
   if oDlgRun <> nil
      oDlgRun:cMsg:="Conectando Banco de Dados em "+cIp
      oDlgRun:Refresh()
   endif
   scriptConectarMysql:="MySQL="+alltrim( cIp )
   scriptConectarMysql+=";UID=" +alltrim( cUser )
   scriptConectarMysql+=";PWD=" +alltrim( cPassword )
   if lChkDatabase
      scriptConectarMysql+=";DTB=" +alltrim( 'mysql' )
   else
      scriptConectarMysql+=";DTB=" +alltrim( cDatabase )
   endif
   scriptConectarMysql+=";PRT=" +alltrim( cPort )
   nConexaoAtual := SR_AddConnection(CONNECT_MYSQL, scriptConectarMysql )
   nTentativas:=0
   While nTentativas<3
      Try
         SR_CheckConnection()
         nTentativas:=3
      catch
         SysWait()
         nTentativas++
         MsgInfo("Tentativa:"+strzero(nTentativas,3,0)+"/3"+CRLF+;
          "Mysql Não Conectado!!"+CRLF+;
          "Dados Da Conexão:"+CRLF+;
          "Erro :"+str(nConexaoAtual)+CRLF+;
          "Host: "+alltrim(cIp)+CRLF+;  //+" "+cPassword+" "+cIp+CRLF+;
          "Porta: "+alltrim(cPort)+CRLF+;
          "Database: "+cDatabase,"Informação")
         if nTentativas=3
            quit
         endif
      end
   end
   temSql := .t.
   RDDSETDEFAULT( "SQLRDD" )
   DBSETDRIVER(   "SQLRDD" )
   SR_SetExclusiveManagement(.t.)

   if lChkDatabase
      oSql   := SR_GetConnection()   // Obtem o objeto da conexão ativa
      IF oSql:Execute("CREATE DATABASE IF NOT EXISTS "+cDatabase) == SQL_SUCCESS
         oSql:Commit()
      ENDIF
      SR_EndConnection( nConexaoAtual )
      ConnectCONFIGMySQL(.t.)
   ENDIF
Function CreateCONFIGMySQL()
   Local oDlgIniSql,lConfirm:=.f.
   Local cHost,cUsu,cSen,cDatabase,cPort
   Store space(30) to cHost,cUsu,cSen,cDatabase,cPort

   if File("SQL.CONFIG")
      fileMysql := MemoRead("SQL.CONFIG")
      aDadosMysql:=HB_ATokens(fileMysql,CRLF)
      if len(aDadosMysql) > 5
         chost:=desprotege(aDadosMysql[1],30)
         cusu:=desprotege(aDadosMysql[2],30)
         cSen:=desprotege(aDadosMysql[3],30)
         cDatabase:=desprotege(aDadosMysql[4],30)
         cPort:=desprotege(aDadosMysql[5],30)
      endif
   endif
    
   lConfirm:= EDITVARS chost,cusu,cSen,cdatabase,cport PROMPTS "Host","User","Pass","Database","Port";
    PICTURES "@!",NIL,NIL,NIL,"9999" VALIDS {||!empty(chost)},{||!empty(cusu)},{||!empty(cSen)},{||!empty(cdatabase)},{||!empty(cport)} Title "Conexão SQL"

   if lConfirm
      cArq := ''
      cArq += protege(cHost,30)+CRLF
      cArq += protege(cUsu,30)+CRLF
      cArq += protege(cSen,30)+CRLF
      cArq += protege(cDatabase,30)+CRLF
      cArq += protege(cPort,30)+CRLF
      MemoWrit("SQL.CONFIG",cArq)
      ConnectCONFIGMySQL(.t.)
   endif
        
function desprotege(cTexto,cChave)
			Decrypt(cTexto,cChave)
function protege(cTexto,cChave)
			Encrypt(cTexto,cChave)	
Function Memo()
			Return SPACE(1000)			
/*
* tdatarow.prg
* FWH 13.05
* Class TDataRow
*
*/


//----------------------------------------------------------------------------//

CLASS TDataRow

   CLASSDATA cBmpList // Sample: "top=restop,up=<bmp>,down=<bmp>,bottom=<bmp>,new=<bmp>,redo=<bmp>,undo=<bmp>,save=,bmp>,close=<bmp>
                      // Case insensitive. Any order. Any number of bmps

   DATA aData
   DATA aOrg   //PROTECTED
   DATA aPrompts
   DATA aModiData
   DATA aDefault
   DATA cTitle
   //
   DATA uSource
   DATA cFieldList
   DATA cSrcType
   DATA cDbms
   DATA l1900  AS LOGICAL INIT .f.
   DATA lReadOnly AS LOGICAL INIT .f.
   DATA lTypeCheck AS LOGICAL INIT .t.
   DATA RecNo AS NUMERIC INIT 0
   DATA bSave, bSaveData, bEdit, bPreSave, bOnSave, bValid
   DATA bGoTop, bGoBottom, bGoUp, bGoDown
   DATA bCanGoUp, bCanGoDn
   //
   DATA cAlias    READONLY INIT ""
   DATA nArea     READONLY INIT 0
   DATA cFile     READONLY INIT ""
   DATA oBrw
   //
   ACCESS lValidData       INLINE ( ! Empty( ::cSrcType ) .and. ! Empty( ::aData ) )
   //
   // Transaction Support
   DATA lUseTrans AS LOGICAL INIT .f.
   DATA bBeginTrans, bCommitTrans, bRollBack
   //
   METHOD New( aData, oDbf ) CONSTRUCTOR
   //
   METHOD FCount           INLINE Len( ::aData )
   MESSAGE FieldPos        METHOD __FieldPos
   METHOD __FieldPos( u )
   METHOD FieldName( n )   INLINE If( n > 0 .and. n < Len( ::aData ), ::aData[ n, 1 ], "" )
   METHOD FieldType( cn )  INLINE ( cn := ::FieldPos( cn ), If( cn > 0, ValType( ::aData[ cn ][ 2 ] ), "U" ) )
   METHOD FieldPrompt( cn ) INLINE ( cn := ::FieldPos( cn ), If( cn > 0, ::aPrompts[ cn ], "" ) )
   METHOD FieldPic( cn, cPic )         // --> cPrev
   METHOD FieldCbxItems( cn, aItems )  // --> aPrev
   METHOD FieldValid( cn, bValid )     // --> bPrev
   METHOD FieldHide( cn, lHide )       // --> lPrev
   //
   METHOD FieldOrg( fld )  INLINE ::aOrg[ ::FieldPos( fld ), 2 ]
   METHOD FieldGet( fld )  INLINE XEval( ::aData[ ::FieldPos( fld ), 2 ] )
   MESSAGE FieldPut        METHOD dr_FieldPut( cnFld, uValue )
   //
   METHOD CopyFrom( oRec )
   METHOD Edit( lReadOnly, lNavigate )
   METHOD Undo( cnFld )
   METHOD Load()
   METHOD Save( lCheckValid )
   METHOD SetPrompt( cnField, cPrompt )
   METHOD SetDefault( ncField, uDefault, lCanModify )
   METHOD Modified( ncField )
   METHOD EditedFlds()
   METHOD CloseMsg()
   METHOD lValid()      INLINE If( ::bValid == nil, .t., Eval( ::bValid, Self ) )
   METHOD End()         INLINE ( ::uSource := nil, ::cSrcType := "", ::aData := nil )
   //
   // Navigataional methods
   //
   METHOD GoTop(l)      INLINE If( ::bGoTop    == nil, nil, If( ::CloseMsg(l), ( Eval( ::bGoTop,    Self ), ::Load() ), nil ) )
   METHOD GoUp(l)       INLINE If( ::bGoUp     == nil, nil, If( ::CloseMsg(l), ( Eval( ::bGoUp,     Self ), ::Load() ), nil ) )
   METHOD GoDown(l)     INLINE If( ::bGoDown   == nil, nil, If( ::CloseMsg(l), ( Eval( ::bGoDown,   Self ), ::Load() ), nil ) )
   METHOD GoBottom(l)   INLINE If( ::bGoBottom == nil, nil, If( ::CloseMsg(l), ( Eval( ::bGoBottom, Self ), ::Load() ), nil ) )
   METHOD GoNew(l)      INLINE If( ::RecNo > 0 .and. ::CloseMsg(l), ::Load( .t. ), nil )
   METHOD CanGoUp       INLINE ::bCanGoUp == nil .or. Eval( ::bCanGoUp )
   METHOD CanGoDn       INLINE ::bCanGoDn == nil .or. Eval( ::bCanGoDn )
   //
   DESTRUCTOR Destroy
   //

PROTECTED:

   METHOD ReadDBF( cFieldList, lBlank )
   METHOD ReadADO( cFieldList, lBlank )
   METHOD ReadDLP( cFieldList, lBlank )
   METHOD ReadObj( cFieldList, lBlank )
   METHOD ReadXBR( cFieldList, lBlank )
   METHOD SaveDbf()
   METHOD SaveADO()
   METHOD SaveDLP()
   METHOD SaveOBJ()
   METHOD SaveXBR()
   METHOD NaviBlocks()
   METHOD BeginTrans()  INLINE If( ::lUseTrans .and. ValType( ::bBeginTrans ) == 'B',  Eval( ::bBeginTans, Self ),  nil )
   METHOD CommitTrans() INLINE If( ::lUseTrans .and. ValType( ::bCommitTrans ) == 'B', Eval( ::bCommitTans, Self ), nil )
   METHOD RollBack()    INLINE If( ::lUseTrans .and. ValType( ::bRollBack ) == 'B',    Eval( ::bRollback, Self ),   nil )
   //
   // Support methods for edit dialog
   METHOD PlaceControls()
   METHOD MakeOneGet()
   METHOD DlgButtons()
   //
   METHOD EqualVal( x, y )
   //
   ERROR HANDLER nomessage

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( uSource, cFieldList, lBlank ) CLASS TDataRow

   local n

   // Parameter tolerance
   if ValType( uSource ) == 'L'
      lBlank := uSource; uSource := nil; cFieldList := nil
   elseif ValType( cFieldList ) == 'L'
      lBlank := cFieldList; cFieldList := nil
   endif
   // Parameter tolerance ends

//   ::aData     := { { "Blank", Space( 20 ) } }

   if uSource == nil
      uSource  := Alias()
   endif

   ::uSource   := uSource
   if ! Empty( cFieldList )
      cFieldList     := TrimList( cFieldList )
      ::cFieldList   := cFieldList
   endif

   ::Load( lBlank )

return Self

//----------------------------------------------------------------------------//

METHOD Load( lBlank ) CLASS TDataRow

   local n, cKey
   local lReload  := ( ValType( ::aData ) == 'A' )

   if ValType( ::uSource ) == 'C' .and. Select( ::uSource ) > 0
      ( ::uSource )->( ::ReadDBF( ::cFieldList, lBlank, lReload ) )
   elseif ValType( ::uSource ) == 'O'
      if ::uSource:ClassName == "TOLEAUTO"
         //
         ::bBeginTrans   := { || ::uSource:ActiveConnection:BeginTrans() }
         ::bCommitTrans  := { || ::uSource:ActiveConnection:CommitTrans() }
         ::bRollBack     := { || ::uSource:ActiveConnection:RollBackTrans() }
         //
         ::ReadADO( ::cFieldList, lBlank, lReload )
      elseif ::uSource:IsKindOf( 'TDOLPHINQRY' )
         ::ReadDLP( ::cFieldList, lBlank, lReload )
      elseif ::uSource:IsKindOf( "TXBROWSE" )
         ::ReadXBR( ::cFieldList, lBlank, lReload )
      else
         ::ReadOBJ( ::cFieldList, lBlank, lReload )
      endif
   elseif ValType( ::uSource ) $ 'AH'
      if ValType( ::uSource ) == 'H'
         ::cSrcType     := "HSH"
         ::aData        := {}
         for each cKey IN ::uSource:Keys
            AAdd( ::aData, { cKey, ::uSource:Values[HB_EnumIndex()] } )
         next
      else
         ::cSrcType     := "ARR"
         ::aData        := ::uSource
      endif
      if ! Empty( ::aData ) .and. ValType( ::aData[ 1 ] ) == 'A' .and. Len( ::aData[ 1 ] ) > 1
         ::aOrg      := AClone( ::aData )
         AEval( ::aOrg, { |a| If( ValType( a[ 2 ] ) == 'B', a[ 2 ] := Eval( a[ 2 ] ), nil ) } )
         for n := 1 to Len( ::aData )
            if Len( ::aData[ n ] ) < 4
               ASize( ::aData[ n ], 4 )
               if ::aData[ n, 3 ] == nil
                  ::aData[ n, 3 ] := .t.
               endif
               if ::aData[ n, 4 ] == nil .and. ValType( ::aData[ n, 2 ] ) == 'N'
                  ::aData[ n, 4 ] := GetNumPict( ::aData[ n, 2 ] )
               endif
            endif
         next
         ::RecNo     := 1
         ::lTypeCheck:= .f.
      endif
   endif

   if ::lValidData
      if ::aPrompts == nil
         ::aPrompts     := {}
         AEval( ::aData, { |a| AAdd( ::aPrompts, a[ 1 ] ) } )
      endif
      if ::aDefault == nil
         ::aDefault     := Array( Len( ::aData ) )
      else
         if ::RecNo == 0
            AEval( ::aDefault, { |u,i| If( u == nil .or. ValType( u ) == 'B', nil, ::aData[ i, 2 ] := u ) } )
         endif
      endif
   else
      // MsgAlert( "Invalid Data" )
   endif

return ::lValidData

//----------------------------------------------------------------------------//

METHOD SetPrompt( ncField, cPrompt ) CLASS TDataRow

   local nPos

   if ValType( ncField ) == 'A'
      // Should be 2-dim array
      AEval( ncField, { |a| ::SetPrompt( a[ 1 ], a[ 2 ] ) } )
      return nil
   elseif ValType( ncField ) == 'N'
      nPos     := ncField
   else
      ncField  := Upper( AllTrim( ncField ) )
      nPos     := AScan( ::aData, { |a| Upper( AllTrim( a[ 1 ] ) ) == ncField } )
   endif
   if nPos < 1 .or. nPos > Len( ::aData )
      return nil
   endif
   cPrompt     := AllTrim( cPrompt )
   if Upper( AllTrim( ::aData[ nPos, 1 ] ) ) == Upper( cPrompt )
      ::aPrompts[ nPos ]   := cPrompt
      return nil
   endif
   if AScan( ::aPrompts, { |c| Upper( c ) == Upper( cPrompt ) } ) == 0 .and. ;
      AScan( ::aData,    { |a| Upper( a[ 1 ] ) == Upper( cPrompt ) } ) == 0
      ::aPrompts[ nPos ]   := cPrompt
   endif

return nil

//----------------------------------------------------------------------------//

METHOD SetDefault( ncField, uDefault, lCanModify ) CLASS TDataRow

   local nPos

   if ValType( ncField ) == 'A'
      // Should be 2-dim array
      AEval( ncField, { |a| ::SetDefault( a[ 1 ], a[ 2 ], If( Len( a ) > 2, a[ 3 ], .t. ) ) } )
      return nil
   elseif ValType( ncField ) == 'N'
      nPos     := ncField
   else
      ncField  := Upper( AllTrim( ncField ) )
      nPos     := AScan( ::aData, { |a| Upper( AllTrim( a[ 1 ] ) ) == ncField } )
   endif
   if nPos < 1 .or. nPos > Len( ::aData )
      return nil
   endif

   DEFAULT lCanModify := ( ValType( uDefault ) != 'B' )

   if ::aData[ nPos, 3 ] == .f.
      lCanModify     := .f.
   endif

   ::aDefault[ nPos ]      := uDefault
   if ::RecNo == 0 .and. ValType( uDefault ) != 'B'
      ::aData[ nPos, 2 ]   := uDefault
   endif
   ::aData[ nPos, 3 ]      := lCanModify

return nil

//----------------------------------------------------------------------------//

METHOD FieldPic( fld, cNewPic ) CLASS TDataRow

   local cPic

   if ( fld := ::FieldPos( fld ) ) > 0
      if Len( ::aData[ fld ] ) >= 4 .and. ValType( ::aData[ fld, 4 ] ) == 'C' .and. Len( ::aData[ fld, 4 ] ) > 1
         cPic     := ::aData[ fld, 4 ]
      endif
      //
      if ValType( cNewPic ) == 'C' .and. Len( cNewPic ) > 1
         if Len( ::aData[ fld ] ) < 4
            ASize( ::aData[ fld ], 5 )
         endif
         ::aData[ fld, 4 ] := cNewPic
      endif
   endif

return cPic

//----------------------------------------------------------------------------//

METHOD FieldCbxItems( fld, aLookUp ) CLASS TDataRow

   local aRet

   if ( fld := ::FieldPos( fld ) ) > 0
      if Len( ::aData[ fld ] ) >= 4 .and. ValType( ::aData[ fld, 4 ] ) == 'A'
         aRet  := ::aData[ fld, 4 ]
      endif
      //
      if ValType( aLookUp ) == 'A' .and. ! Empty( aLookUp )
         if Len( ::aData[ fld ] ) < 4
            ASize( ::aData[ fld ], 5 )
         endif
         ::aData[ fld, 4 ] := aLookUp
      endif
   endif

return aRet

//----------------------------------------------------------------------------//

METHOD FieldValid( fld, bValid ) CLASS TDataRow

   local bRet

   if ( fld := ::FieldPos( fld ) ) > 0
      if Len( ::aData[ fld ] ) >= 5 .and. ValType( ::aData[ fld, 4 ] ) == 'B'
         bRet  := ::aData[ fld, 5 ]
      endif
      //
      if ValType( bValid ) == 'B'
         if Len( ::aData[ fld ] ) < 5
            ASize( ::aData[ fld ], 5 )
         endif
         ::aData[ fld, 5 ] := bValid
      endif
   endif

return bRet

//----------------------------------------------------------------------------//

METHOD FieldHide( fld, lHide ) CLASS TDataRow

   local lRet := .f.

   if ( fld := ::FieldPos( fld ) ) > 0
      if Len( ::aData[ fld ] ) >= FLD_HIDE
         lRet     := ( ::aData[ fld, FLD_HIDE ] == .t. )
      endif
      //
      if ValType( lHide ) == 'L'
         if Len( ::aData[ fld ] ) >= FLD_HIDE
            ::aData[ fld, FLD_HIDE ]   := lHide
         elseif lHide
            ASize( ::aData[ fld ], FLD_HIDE )
            ::aData[ fld, FLD_HIDE ]   := lHide
         endif
      endif
   endif

return lRet

//----------------------------------------------------------------------------//

METHOD dr_FieldPut( cnfld, uVal ) CLASS TDataRow

   local fld, cFldType, cType

   if ( fld := ::FieldPos( cnfld ) ) > 0
      if ::lReadOnly .or. ( Len( ::aData[ fld ] ) > 2 .and. ::aData[ fld, 3 ] == .f. )
         return ::error(  HB_LangErrMsg( 39 ), ::className(), ::FieldName( fld ), 39, { uVal } )
      else
         cFldType    := ValType( ::aOrg[ fld, 2 ] )
         cType       := ValType( uVal )
         if cFldType == 'D' .and. cType == 'T'
            uVal     := FW_TTOD( uVal )
         elseif cFldType == 'T' .and. cType == 'D'
            uVal     := FW_DTOT( uVal )
         endif
         if ! ::lTypeCheck .or. ::aOrg[ fld, 2 ] == nil .or. ;
            ValType( uVal ) == cFldType

            if ValType( ::aData[ fld, 2 ] ) == 'B'
               Eval( ::aData[ fld, 2 ], uVal )
            else
               ::aData[ fld, 2 ]    := uVal
            endif
            return ::FieldGet( fld )
         else
            return ::error(  HB_LangErrMsg( 33 ), ::className(), ::FieldName( fld ), 33, { uVal } )
         endif
      endif
   else
      return ::error(  HB_LangErrMsg( 14 ), ::className(), cValToChar( cnfld ), 14, { uVal } )
   endif

return nil

//----------------------------------------------------------------------------//

METHOD Modified( fld ) CLASS TDataRow

   local lModified   := .f.

   if PCount() > 0
      if ( fld := ::FieldPos( fld ) ) > 0
         lModified := ! ::EqualVal( ::aData[ fld, FLD_VAL ], ::aOrg[ fld, FLD_VAL ] )
         if lModified .and. ::RecNo == 0 .and. ! Empty( ::aDefault[ fld ] )
            if ::EqualVal( ::aData[ fld, FLD_VAL ], XEval( ::aDefault[ fld ] ) )
               lModified    := .f.
            endif
         endif
      endif
   else
      for fld := 1 to Len( ::aData )
         if ::Modified( fld )
            lModified   := .t.
            exit
         endif
      next
   endif

return lModified

//----------------------------------------------------------------------------//

METHOD Undo( fld ) CLASS TDataRow

   if PCount() > 0
      if ( fld := ::FieldPos( fld ) ) > 0
         if ValType( ::aData[ fld, 2 ] ) == 'B'
            Eval( ::aData[ fld, 2 ], ::aOrg[ fld, 2 ] )
         else
            ::aData[ fld, 2 ] := ::aOrg[ fld, 2 ]
         endif
      endif
   else
      AEval( ::aData, { |a,i| If( ValType( a[ 2 ] ) == 'B', ;
                  Eval( a[ 2 ], ::aOrg[ i, 2 ] ), ;
                  a[ 2 ] := ::aOrg[ i, 2 ] ) } )
   endif

return nil

//----------------------------------------------------------------------------//

METHOD EditedFlds() CLASS TDataRow

   local n, aRet  := {}

   for n := 1 to Len( ::aData )
      if ! ::EqualVal( ::aData[ n, 2 ], IfNil( XEval( ::aDefault[ n ] ), ::aOrg[ n, 2 ] ) )
         AAdd( aRet, { n, ::aData[ n, 1 ], ::aData[ n, 2 ], ::aOrg[ n, 2 ], ;
                       XEval( ::aDefault[ n ] ) } )
      endif
   next n

return aRet

//----------------------------------------------------------------------------//

METHOD CloseMsg( lSave ) CLASS TDataRow

   local nChoice  := 0
   local a

   nChoice  := If( lSave == .t., 1, If( lSave == .f., 2, 0 ) )

   if ::cSrcType $ "ARR"
      AEval( ::aOrg, { |a,i| ASize( ::aData[ i ], Len( a ) ) } )
   elseif ::cSrcType $ "HSH"
      for each a in ::aData
         ::uSource[ a[ 1 ] ] := a[ 2 ]
      next
   elseif ! ::lReadOnly .and. ::Modified()
      if nChoice == 0
         nChoice  := Alert( "Data Modified. Save/Discard Changes?", { "Save", "Discard", "Cancel" } )
      endif
      if nChoice == 1
         ::Save()
      elseif nChoice == 2
         ::Undo()
      else
         return .f.
      endif
   endif

return .t.

//----------------------------------------------------------------------------//

METHOD __FieldPos( cName ) CLASS TDataRow

   local nPos     := 0

   if ValType( cName ) == 'N'
      return cName
   endif
   cName    := Upper( AllTrim( cName ) )
   if ( nPos := AScan( ::aPrompts, { |c| Upper( c ) == cName } ) ) == 0
   if ( nPos := AScan( ::aPrompts, { |c| Upper( CharRem( ' ', c ) ) == cName } ) ) == 0
   if ( nPos := AScan( ::aPrompts, { |c| Upper( StrTran( c, ' ', '_' ) ) == cName } ) ) == 0
      nPos  := AScan( ::aData, { |a| Upper( Trim( a[ 1 ] ) ) == cName } )
   endif
   endif
   endif

return nPos

//----------------------------------------------------------------------------//

METHOD ReadDBF( cFieldList, lBlank, lReload ) CLASS TDataRow

   local i, j, nFieldPos

   DEFAULT cFieldList   := FW_ArrayAsList( ArrTranspose( DbStruct() )[ 1 ] ), ;
           lBlank       := Eof()

   ::cAlias    := ::uSource
   ::nArea     := Select( ::cAlias )
   ::cFile     := ( ::nArea )->( DBINFO( DBI_FULLPATH ) )

   if ValType( cFieldList ) == 'A'
      cFieldList  := FW_ArrayAsList( cFieldList )
   endif
   cFieldList  := StrTran( cFieldList, ' ', '' )

   if lReload
      AEval( Eval( &( "{ || { " + cFieldList + " } }" ) ), { |u,i| ::aData[ i, 2 ] := u } )
   else
      ::aData  := ArrTranspose( { FW_ListAsArray( cFieldList ), Eval( &( "{ || { " + cFieldList + " } }" ) ) } )
   endif

   if lBlank .and. ! eof()
      AEval( ::aData, { |a| a[ 2 ] := uValBlank( a[ 2 ] ) } )
   endif

   if ! lReload
      for i := 1 to Len( ::aData )
         ASize( ::aData[ i ], 4 )
         nFieldPos   := FieldPos( ::aData[ i, 1 ] )
         ::aData[ i, 3 ]   := !( FieldType( nFieldPos ) $ "+=" )
         if FieldType( nFieldPos  ) $ 'N+'
            ::aData[ i, 4 ]   := NumPict( FieldLen( nFieldPos ), FieldDec( nFieldPos ) )
         else
            ::aData[ i, 4 ]   := FieldType( nFieldPos )
         endif
      next
   endif

   ::aOrg      := AClone( ::aData )
   ::RecNo     := If( lBlank .or. eof(), 0, RecNo() )
   ::cSrcType  := "DBF"
   ::lReadOnly := DbInfo( DBI_ISREADONLY )
   if Empty( ::cTitle )
      ::cTitle := cFileNoExt( DbInfo( DBI_FULLPATH ) )
   endif

return nil

//----------------------------------------------------------------------------//

METHOD ReadADO( cFieldList, lBlank, lReload ) CLASS TDataRow

   local oRs      := ::uSource
   local n, nFlds, aList, aFld, aData := {}
   local oField, cType, uVal, cPic

   DEFAULT lBlank := oRs:Eof()

   if ! lBlank
      TRY
         oRs:Resync( adAffectCurrent, adResyncAllValues )
      CATCH
      END
   endif

   DEFAULT ::cDBMS := FW_RDBMSName( oRs:ActiveConnection )
   ::l1900         := ! Empty( ::cDbms ) .and. ::cDbms $ "MSACCESS,MSSQL"

   if cFieldList == nil
      nFlds := oRs:Fields:Count
      aFld  := Array( nFlds )
      for n := 1 to nFlds
         aFld[ n ]   := oRs:Fields:Item( n - 1 )
      next
   else
      aList    := HB_ATokens( cFieldList, ',' )
      aFld     := Array( Len( aList ) )
      for n := 1 to Len( aList )
         TRY
            aFld[ n ]   := oRs:Fields( aList[ n ] )
         CATCH
            // invalid field name
         END
      next
   endif
   for each oField in aFld
      if oField != nil
         cType    := FieldTypeAdoToDbf( oField:Type )
         cPic     := nil
         if ! Empty( cType ) .and. cType $ "CDLNT"
            uVal     := If( oRs:Eof(), nil, oField:Value )
            if ValType( uVal ) == 'D' .and. uVal == {^ 1899/12/30 }
               uVal  := nil
            elseif ValType( uVal ) == 'T'
               if Left( HB_TTOS( uVal ), 8 ) == "18991230"
                  uVal  := nil
               elseif FW_TIMEPART( uVal ) < 1.0
                  uVal  := FW_TTOD( uVal )
               endif
            endif
            if uVal == nil .or. lBlank
               uVal  := FW_DeCode( cType, 'C', Space( Min( 100, oField:DefinedSize ) ), ;
                        'D', CToD( '' ), 'L', .f., 'N', 0.00, 'T', CToT( '' ), nil )
            else
               if cType == 'C' .and. ! IsBinaryData( uVal )
                  uVal     := Trim( uVal )
                  if Len( uVal ) < Min( 100, oField:DefinedSize )
                     uVal  := PadR( uVal, Min( 100, oField:DefinedSize ) )
                  endif
               endif
            endif
            if cType == 'N'
               if AScan( { 14, 131, 139 }, oField:Type ) > 0
                  cPic  := NumPict( Min( 19, oField:Precision ), ;
                           If( oField:NumericScale >= 255, 0, ;
                              Min( Min( 19, oField:Precision ) - 2, oField:NumericScale ) ) )
               elseif AScan( { 4, 5, 6 }, oField:Type ) > 0
                  cPic  := NumPict( Min( oField:Precision, 11 ), 2 )
               elseif AScan( { 2,3,16,17,18,19,20,21 }, oField:Type ) > 0
                  cPic  := NumPict( Min( oField:Precision, 10 ), 0 )
               endif
            endif
            if cType == 'C' .and. oField:DefinedSize > 100
               cType    := 'M'
            endif
            AAdd( aData, { oField:Name, uVal, FW_AdoFieldUpdateable( oRs, oField ), IfNil( cPic, cType ) } )
         endif
      endif
   next

   if lReload
      AEval( aData, { |a,i| ::aData[ i, 2 ] := a[ 2 ] } )
   else
      ::aData     := aData
   endif
   ::RecNo     := If( lBlank, 0, oRs:BookMark )
   ::aOrg      := AClone( ::aData )
   ::cSrcType  := "ADO"
   ::lReadOnly := ( oRs:LockType == adLockReadOnly )
   if ! Empty( oRs:ActiveConnection ) .and. FW_RDBMSName( oRs:ActiveConnection ) = "SQLITE"
      ::lTypeCheck   := .f.
   endif

   if Empty( ::cTitle )
      TRY
         ::cTitle := ::oRs:Fields( 0 ):Properties( "BASETABLENAME" ):Value
      CATCH
      END
   endif

return nil

//----------------------------------------------------------------------------//

METHOD ReadDLP( cFieldList, lBlank, lReload ) CLASS TDataRow

   local oQry      := ::uSource
   local n, nFld, nFlds, aList, aFld, aData := {}
   local oField, cType, uVal, cPic

   DEFAULT lBlank := oQry:Eof()

   if cFieldList == nil
      nFlds := oQry:FCount()
      aFld  := Array( nFlds )
      AEval( aFld, { |u,i| aFld[ i ] := i } )
   else
      aList    := HB_ATokens( cFieldList, ',' )
      aFld     := Array( Len( aList ) )
      for n := 1 to Len( aList )
         aFld[ n ] := oQry:FieldPos( aList[ n ] )
      next
   endif
   for n := 1 to Len( aFld  )
      nFld     := aFld[ n ]
      cType    := oQry:FieldType( nFld )
      uVal     := oQry:FieldGet( nFld )

      AAdd( aData, { oQry:FieldName( nFld ), ;
         If( lBlank, uValBlank( uVal ), uVal ), ;
         .t., ;
         If( cType == 'N', NumPict( oQry:FieldLen( nFld ), oQry:FieldDec( nFld ) ), cType ), ;
         nil } )
   next

   if lReload
      AEval( aData, { |a,i| ::aData[ i, 2 ] := a[ 2 ] } )
   else
      ::aData     := aData
   endif
   ::RecNo     := If( lBlank, 0, oQry:RecNo() )
   ::aOrg      := AClone( ::aData )
   ::cSrcType  := "DLP"
   ::lReadOnly := .f.
   ::lTypeCheck   := .t.

return nil

//----------------------------------------------------------------------------//

METHOD ReadOBJ( cFieldList, lBlank, lReload )

   local aData

   if __ObjHasMethod( ::uSource, "ROWGET" )
      aData       := ::uSource:RowGet( cFieldList, @lBlank )
      if Empty( ::aData )
         ::aData  := aData
         ::aOrg   := AClone( ::aData )
      else
         AEval( aData, { |a,i| ::aData[ i, 2 ] := a[ 2 ], ::aOrg[ i, 2 ] := a[ 2 ] } )
      endif
      ::RecNo     := If( lBlank, 0, ::uSource:RecNo() )
      ::cSrcType  := "OBJ"
      ::lReadOnly := ::uSource:lReadOnly
      return nil
   endif

return nil

//----------------------------------------------------------------------------//

METHOD ReadXBR( cFieldList, lBlank ) CLASS TDataRow

   local oBrw     := ::uSource
   local aHeaders, cHeader, oCol

   DEFAULT lBlank := ( oBrw:nLen < 1 )

   if cFieldList == nil
      aHeaders    := oBrw:cHeaders
   else
      aHeaders    := HB_ATokens( cFieldList, "," )
      AEval( aHeaders , { |c,i| aHeaders[ i ] := AllTrim( c ) } )
   endif

   ::aData     := {}
   for each cHeader in aHeaders
      oCol     := oBrw:oCol( cHeader )
      if oCol != nil
         AAdd( ::aData, { cHeader, If( lBlank, oCol:BlankValue(), oCol:Value ), oCol:lEditable, nil } )
         if ValType( ATail( ::aData )[ 2 ] ) == 'N'
            ATail( ::aData )[ 4 ] := If( Empty( oCol:cEditPicture ), NumPict( 12, 2 ), oCol:cEditPicture )
         else
            ATail( ::aData )[ 4 ] := oCol:cDataType
            if oCol:bEditBlock != nil
               ATail( ::aData )[ 4 ] := oCol:bEditBlock
            elseif oCol:nEditType == EDIT_LISTBOX
               ATail( ::aData )[ 4 ] := ArrTranspose( { oCol:aEditListBound, oCol:aEditListTxt } )
            endif
         endif
      endif
   next
   ::RecNo     := If( lBlank, 0, oBrw:BookMark )
   ::aOrg      := AClone( ::aData )
   ::cSrcType  := "XBR"
   ::lReadOnly := oBrw:lReadOnly
   if oBrw:nDataType == DATATYPE_ARRAY
      ::lTypeCheck   := .f.
   elseif oBrw:nDataType == DATATYPE_ADO
      if ! Empty( oBrw:oRs:ActiveConnection ) .and. FW_RDBMSName( oBrw:oRs:ActiveConnection ) = "SQLITE"
         ::lTypeCheck   := .f.
      endif
      ::bBeginTrans   := { || ::uSource:oRs:ActiveConnection:BeginTrans() }
      ::bCommitTrans  := { || ::uSource:oRs:ActiveConnection:CommitTrans() }
      ::bRollBack     := { || ::uSource:oRs:ActiveConnection:RollBackTrans() }

   endif

return nil

//----------------------------------------------------------------------------//

METHOD Save( lCheckValid ) CLASS TDataRow

   local lSaved   := .f.
   local n, lAdded := .f.

   if lCheckValid == .t.
      if .not. ::lValid()
         MsgInfo( "Can not save Invalid Data" )
         return .f.
      endif
   endif

   ::BeginTrans()

   ::aModiData    := {}
   for n := 1 to ::FCount()
      if ( ::aData[ n, 3 ] == nil .or. ::aData[ n, 3 ] ) .and. ::Modified( n )
         AAdd( ::aModiData, { ::aData[ n, 1 ], ::aData[ n, 2 ], n } )
      endif
   next

   if ::bSave != nil
      lSaved   := Eval( ::bSave, Self )
      lSaved   := If( ValType( lSaved ) == 'L', lSaved, .t. )
   elseif ! Empty( ::aModiData )
      if ::RecNo == 0
         for n := 1 to Len( ::aDefault )
            if ValType( ::aDefault[ n ] ) == 'B'
               if AScan( ::aModiData, { |a| a[ 3 ] == n } ) == 0
                  AAdd( ::aModiData, { ::aData[ n, 1 ], XEval( ::aDefault[ n ] ), n } )
                  lAdded := .t.
               endif
            endif
         next n
      endif
      if lAdded
         ASort( ::aModiData, nil, nil, { |x,y| x[ 3 ] < y[ 3 ] } )
      endif

      if ::bPreSave != nil
         if ::cSrcType == "DBF"
            ( ::uSource )->( Eval( ::bPreSave, Self ) )
         else
            Eval( ::bPreSave, Self )
         endif
        ::aModiData    := {}
        for n := 1 to ::FCount()
           if ( ::aData[ n, 3 ] == nil .or. ::aData[ n, 3 ] ) .and. ::Modified( n )
              AAdd( ::aModiData, { ::aData[ n, 1 ], ::aData[ n, 2 ], n } )
           endif
        next
      endif
      if ::cSrcType == "DBF"
         lSaved   := ( ::uSource )->( ::SaveDBF() )
      elseif ::cSrcType == "ADO"
         lSaved   := ::SaveADO()
      elseif ::cSrcType == "DLP"
         lSaved   := ::SaveDLP()
      elseif ::cSrcType == "OBJ"
         lSaved   := ::SaveOBJ()
      elseif ::cSrcType == "XBR"
         lSaved   := ::SaveXBR()
      endif
   endif

   if lSaved .and. ::bOnSave != nil
      lSaved   := Eval( ::bOnSave, Self )
      lSaved   := If( ValType( lSaved ) == 'L', lSaved, .t. )
   endif

   if lSaved
      ::CommitTrans()
   else
      ::RollBack()
   endif

return lSaved

//----------------------------------------------------------------------------//

METHOD CopyFrom( oRec ) CLASS TDataRow

   local aField

   for each aField in oRec:aData
      TRY
         ::FieldPut( aField[ 1 ], aField[ 2 ] )
      CATCH
      END
   next

return nil

//----------------------------------------------------------------------------//

METHOD SaveDBF() CLASS TDataRow

   local n, nCols    := ::FCount()
   local lAppend     := ( ::RecNo == 0 .or. ( bof() .and. eof() ) )
   local nSaveRec

   if ::lReadOnly .or. ! ::Modified()
      return .f.
   endif

   if lAppend
      ::RecNo  := 0
      REPEAT
         DbAppend()
      UNTIL ! NetErr()
   else
      if ::RecNo != RecNo()
         nSaveRec    := RecNo()
      endif
      DbGoTo( ::RecNo )
      do while ! DbRLock(); enddo
   endif

   if ::bSaveData == nil
      for n := 1 to Len( ::aModiData )
         TRY
            FieldPut( FieldPos( ::aModiData[ n, 1 ] ), ::aModiData[ n, 2 ] )
         CATCH
         END
      next n
   else
      Eval( ::bSaveData, Self )
   endif
   DbUnlock()

   ::ReadDBF( FW_ArrayAsList( ArrTranspose( ::aData )[ 1 ] ), .f., .t. )

   if ! Empty( nSaveRec )
      DbGoTo( nSaveRec )
   endif

return .t.

//----------------------------------------------------------------------------//

METHOD SaveADO() CLASS TDataRow

   local oRs   := ::uSource
   local lAppend  := ( ::RecNo == 0 .or. ( oRs:Bof .and. oRs:Eof ) )
   local uSaveBm, a, uVal, oField, n, aCols, aVals
   local lSaved   := .f.
   local lBinary  := .f.

   if ::lReadOnly .or. ! ::Modified()
      return .f.
   endif

   if lAppend
      ::RecNo     := 0
   elseif ::RecNo != oRs:BookMark
      uSaveBm        := oRs:BookMark
      oRs:BookMark   := ::RecNo
   endif

   if ::bSaveData == nil
      for n := 1 to Len( ::aModiData )
         oField   := oRs:Fields( ::aModiData[ n, 1 ] )
         lBinary  := AScan( { 128, 204, 205 }, oField:Type ) > 0
         uVal     := ::aModiData[ n, 2 ]
         if ValType( uVal ) == 'C'
            if lBinary
               uVal  := HB_STRTOHEX( uVal )
            else
               uVal  := Left( Trim( uVal ), oField:DefinedSize )
            endif
         endif
         if ValType( uVal ) $ "DT"
            if Empty( uVal )
               uVal     := AdoNull()
            elseif Year( uVal ) < 1900 .and. ::l1900
               uVal     := AdoNull()
            endif
         endif
         ::aModiData[ n, 2 ]  := uVal
      next n

      a     := ArrTranspose( ::aModiData )
      aCols := a[ 1 ]
      aVals := a[ 2 ]

      TRY
         if lAppend
            oRs:AddNew( aCols, aVals )
         else
            oRs:Update( aCols, aVals )
         endif
         lSaved   := .t.
      CATCH
         FW_ShowAdoError( oRs:ActiveConnection )
         oRs:CancelUpdate()
      END
   else
      lSaved   := Eval( ::bSaveData, Self )
      lSaved   := If( ValType( lSaved ) == 'L', lSaved, .t. )
   endif
   if lSaved
      ::ReadADO( FW_ArrayAsList( ArrTranspose( ::aData )[ 1 ] ), .f., .t. )
      if ! lAppend .and. uSaveBm != nil
         oRs:BookMark   := uSaveBm
      endif                                                                                                       a[ 2
   else
      ::Undo()
   endif

return lSaved

//----------------------------------------------------------------------------//

METHOD SaveDLP() CLASS TDataRow

   local n, nCols    := ::FCount()
   local oQry        := ::uSource
   local nSaveRec
   local lAppend     := ( ::RecNo == 0 )

   if ::lReadOnly .or. ! ::Modified()
      return .f.
   endif

   if lAppend
      if ! oQry:lAppend
         oQry:GetBlankRow( .f. )
      endif
   else
      nSaveRec    := ::RecNo
   endif

   if ::bSaveData == nil
      for n := 1 to Len( ::aModiData )
         oQry:FieldPut( oQry:FieldPos( ::aModiData[ n, 1 ] ), ::aModiData[ n, 2 ] )
      next n
      oQry:Save()
      if lAppend
         oQry:Refresh()
      endif
   else
      Eval( ::bSaveData, Self )
   endif

   if lAppend
      n     := ArrTranspose( ::aModiData )
      oQry:Find( n[ 2 ], n[ 1 ] )
   endif

   ::ReadDLP( FW_ArrayAsList( ArrTranspose( ::aData )[ 1 ] ), .f., .t. )

   if ! Empty( nSaveRec )
      oQry:GoTo( nSaveRec )
   endif

return .t.

//----------------------------------------------------------------------------//

METHOD SaveXBR() CLASS TDataRow

   local oBrw     := ::uSource
   local lAppend  := ( ::RecNo == 0 .or. oBrw:nLen < 1 )
   local uSaveBm  := oBrw:BookMark
   local nRow, aRow
   local lSaved   := .f.

   if ::lReadOnly .or. ! ::Modified()
      return .f.
   endif

   if lAppend
      if ! XBrAddNewRow( oBrw )
         MsgAlert( "Can not append new row for the datasource" )
         return .f.
      endif
   elseif ::RecNo != oBrw:BookMark
      uSaveBm        := oBrw:BookMark
      oBrw:BookMark  := ::RecNo
   endif
   if ::bSaveData == nil
      for nRow := 1 to Len( ::aData )
         aRow  := ::aData[ nRow ]
         // if aRow[ 3 ] .and. ::Modified( nRow )
         if aRow[ 3 ] .and. ( lAppend .or. ::Modified( nRow ) ) // 2014-04-11. Write all fields while appending
            oBrw:oCol( aRow[ 1 ] ):VarPut( aRow[ 2 ] )
            lSaved   := .t.
         endif
      next
   else
      lSaved   := Eval( ::bSaveData, Self )
      lSaved   := If( ValType( lSaved ) == 'L', lSaved, .t. )
   endif

   if lAppend .and. lSaved
      lSaved   := XbrSaveNewRow( oBrw )
   endif

   if lSaved
      ::ReadXBR( FW_ArrayAsList( ArrTranspose( ::aData )[ 1 ] ), .f., .t. )
      if ! lAppend .and. uSaveBm != nil
         oBrw:BookMark   := uSaveBm
      endif
   else
      ::Undo()
   endif
   oBrw:Refresh()

return lSaved

//----------------------------------------------------------------------------//

METHOD SaveOBJ() CLASS TDataRow

   if ::lReadOnly .or. ! ::Modified()
      return .f.
   endif

   if __ObjHasMethod( ::uSource, "ROWPUT" )
      ::RecNo  := ::uSource:RowPut( ::aData, ::RecNo, .f., Self )
      ::aOrg   := AClone( ::aData )
   endif

return .t.

//----------------------------------------------------------------------------//

METHOD NaviBlocks() CLASS TDataRow

   if ::bGoTop != nil
      return nil
   endif

   if ::oBrw != nil
      ::bGoTop       := { |oRec| oRec:oBrw:GoTop() }
      ::bGoUp        := { |oRec| oRec:oBrw:GoUp() }
      ::bGoDown      := { |oRec| oRec:oBrw:GoDown() }
      ::bGoBottom    := { |oRec| oRec:oBrw:GoBottom() }
      ::bCanGoUp     := { || ::oBrw:KeyNo > 1 }
      ::bCanGoDn     := { || ::oBrw:KeyNo < ::oBrw:nLen }
   elseif ::cSrcType == "DBF"
      ::bGoTop       := { |oRec| ( oRec:uSource )->( DbGoTop() ) }
      ::bGoUp        := { |oRec| ( oRec:uSource )->( DbSkip( -1 ), If( Bof(), DbGoTop(), nil ) ) }
      ::bGoDown      := { |oRec| ( oRec:uSource )->( DbSkip( +1 ), If( Eof(), DbGoBottom(), nil ) ) }
      ::bGoBottom    := { |oRec| ( oRec:uSource )->( DbGoBottom() ) }
      ::bCanGoUp     := { || ( ::uSource )->( OrdKeyNo() ) > 1 }
      ::bCanGoDn     := { || ( ::uSource )->( OrdKeyNo() ) < ( ::uSource )->( OrdKeyCount() ) }
   elseif ::cSrcType == "ADO"
      ::bGoTop       := { |oRec| If( oRec:uSource:RecordCount() > 0, oRec:uSource:MoveFirst(), nil ) }
      ::bGoUp        := { |oRec| If( oRec:uSource:RecordCount() > 0 .and. oRec:uSource:AbsolutePosition > 1, ;
                                     oRec:uSource:MovePrevious(), nil ) }
      ::bGoDown      := { |oRec| If( oRec:uSource:RecordCount() > 0 .and. oRec:uSource:AbsolutePosition < oRec:uSource:RecordCount(), ;
                                     oRec:uSource:MoveNext(), nil ) }
      ::bGoBottom    := { |oRec| If( oRec:uSource:RecordCount() > 0, oRec:uSource:MoveLast(), nil ) }
      ::bCanGoUp     := { || ::uSource:RecordCount() > 0 .and. ::uSource:AbsolutePosition > 1 }
      ::bCanGoDn     := { || ! ::uSource:Eof() .and. ::uSource:AbsolutePosition < ::uSource:RecordCount() }
   elseif ::cSrcType $ "OBJ,DLP"
      ::bGoTop       := { |oRec| oRec:uSource:GoTop() }
      ::bGoUp        := { |oRec| oRec:uSource:Skip( -1 ) }
      ::bGoDown      := { |oRec| oRec:uSource:Skip( +1 ) }
      ::bGoBottom    := { |oRec| oRec:uSource:GoBottom() }
      if ::cSrcType == "OBJ"
         ::bCanGoUp     := { || ::uSource:KeyNo() > 1 }
         ::bCanGoDn     := { || ::uSource:KeyNo() < ::uSource:KeyCount() }
      else
         ::bCanGoUp     := { || ::uSource:RecNo() > 1 }
         ::bCanGoDn     := { || ::uSource:RecNo() < ::uSource:RecCount() }
      endif
   endif

return nil

//----------------------------------------------------------------------------//

METHOD Edit( lReadOnly, lNavigate, cTitle, cMsg ) CLASS TDataRow

   local oRec  := Self
   local oDlg, oPanel
   local oFont, oSayFont, oFixed
   local uRet

   if ! ::lValidData
      return .f.
   endif

   ::NaviBlocks()

   if ::bEdit != nil
      uRet  := Eval( ::bEdit, Self )
      return uRet
   endif

   DEFAULT lReadOnly := ::lReadOnly //.and. Empty( ::bSave )
   DEFAULT lNavigate := .t.
   DEFAULT cTitle := FWString( If( lReadOnly, "VIEW", If( ::RecNo == 0, "NEW", "EDIT" ) ) ) + ;
      ' ' + If( Empty( ::cTitle ), "DATA", ::cTitle )


   DEFINE FONT oFont NAME "TAHOMA" SIZE 0,-14
   oSayFont := oFont:Bold()
   DEFINE FONT oFixed  NAME "COURIER NEW" SIZE 0,-16

   DEFINE DIALOG oDlg SIZE 800,500 PIXEL FONT oFont TITLE cTitle
   oPanel   := TScrollPanel():New( 20, 20, 200, 360, oDlg, .t. )
   oPanel:SetColor( CLR_BLACK, oDlg:nClrPane )
   oPanel:SetFont( oDlg:oFont )

   lExit    := .f.
   ACTIVATE DIALOG oDlg CENTERED ;
      ON INIT oRec:PlaceControls( oPanel, oSayFont, oFixed, lReadOnly, lNavigate, cMsg ) ;
      VALID ( !GetKeyState(27) )
   RELEASE FONT oFont, oFixed, oSayFont

   if ::cSrcType == 'XBR'
      ::uSource:SetFocus()
   elseif ::oBrw != nil
      ::oBrw:SetFocus()
   endif

return lExit // ::Modified()  // return value valid only for arrays

//----------------------------------------------------------------------------//

METHOD PlaceControls( oPanel, oSayFont, oFixed, lReadOnly, lNavigate, cMsg ) CLASS TDataRow

   local nItem, oSay, oGet, nGets := 0
   local nRow  := 20
   local nSayWidth, nGetWidth, nWidth
   local nMaxSayWidth   := 0
   local nMaxGetWidth   := 0
   local oDlg     := oPanel:oWnd
   local hDC      := oDlg:GetDC()
   local nPanelWidth, nPanelHeight

   for nItem := 1 to Len( ::aData )
      if ! ::FieldHide( nItem )
         if LEFT(UPPER(::aPrompts[ nItem ]),4) # "SAY:"
				nMaxSayWidth   := Max( nMaxSayWidth, GetTextWidth( hDC, ::aPrompts[ nItem ] + " :", oSayFont:hFont ) )
      	endif
      endif
   next

   for nItem := 1 to Len( ::aData )
      if ! ::FieldHide( nItem )
         ::MakeOneGet( hDC, oPanel, @nRow, nItem, lReadOnly, oSayFont, oFixed, nMaxSayWidth, @nGetWidth )
         nMaxGetWidth   := Max( nMaxGetWidth, nGetWidth )
      endif
   next

   nGets    := 0
   for nItem := 2 to Len( oPanel:aControls ) STEP 2
      oGet     := oPanel:aControls[ nItem ]
      if oGet:ClassName() == "TMULTIGET"
         oGet:nWidth := nMaxSayWidth + 10 + nMaxGetWidth
      endif
      nGets++
   next
   oDlg:ReleaseDC()
   oPanel:SetRange()

   nPanelWidth    := Max( If( lNavigate, 400, 200 ), 20 + nMaxSayWidth + 10 + nMaxGetWidth + 20 + 24 )
   nPanelHeight   := Min(  Int( ScreenHeight() * 0.8 - 100 ), ;
                           ATail( oPanel:aControls ):nBottom + 20 )

   oDlg:SetCoors(   TRect():new(  0,  0, 40 + nPanelHeight + 100, 40 + nPanelWidth + 40  ) )
   oPanel:SetCoors( TRect():New( 40, 40, 40 + nPanelHeight,      40 + nPanelWidth       ) )
   oDlg:bPainted  := { || oDlg:Box( oPanel:nTop - 2, oPanel:nLeft - 1, oPanel:nBottom + 2, oPanel:nRight + 2 ) }

   if nPanelHeight >= ATail( oPanel:aControls ):nBottom
      oPanel:nScrollRange     := 0
      oPanel:oVScroll:SetRange( 0, 0 )
      oPanel:WinStyle( WS_VSCROLL, .f. )
   endif

   ::DlgButtons( oDlg, oPanel, lNavigate, lReadOnly, nGets )

   if ! Empty( cMsg )
      @ 06,00 SAY cValToChar( cMsg ) SIZE oDlg:nWidth, 20 PIXEL OF oDlg CENTER TRANSPARENT
   endif

   oDlg:Center()
   oPanel:aControls[ 2 ]:SetFocus()

return .f.

//----------------------------------------------------------------------------//

METHOD MakeOneGet( hDC, oPanel, nRow, nAt, lReadOnly, oSayFont, oFixed, nSayWidth, nGetWidth ) CLASS TDataRow

#define ROWHT  22
#define ROWGAP  4

   local oRec     := Self
   local aLine, oGet, bGet, bValid, bAction
   local lCheck, lCombo, lMemo, cType, cPic, cVal
   local lPassWord   := .f.
   local nCol     := 20 + nSayWidth + 10
   local cBMP:= ""
	local lSayDescription := .f.
	Local aSay,bSay,eSay,cSay,nSayGetWidth,oSay
	
		if LEN(::aPrompts) >= nAt+1
			if upper(left(::aPrompts[ nAt+1 ],4)) == "SAY:"
				lSayDescription := .t.
			endif
		endif	


   aLine       := AClone( ::aData[ nAt ] )
   ASize( aLine, 5 )
   DEFAULT aLine[ 3 ] := .t.

   if ValType( aLine[ 2 ] ) == 'B'
      bGet     := aLine[ 2 ]
      cType    := ValType( Eval( bGet ) )
   else
      cType       := ValType( IfNil( aLine[ 2 ], Space( 60 ) ) )
   endif

   lCheck      := ( cType == 'L' )
   lCombo      := ValType( aLine[ 4 ] ) == 'A'
   lMemo       := .f.
	if ValType( aLine[ 4 ] ) == 'C' 
		lMemo := Upper( aLine[ 4 ] ) == 'M' 
	endif	
		
   if ::lReadOnly .or. ( aLine[ 3 ] == .f. )
      lReadOnly   := .t.
   endif

   if lReadOnly
      DEFAULT bGet := { || IfNil( oRec:aData[ nAt, 2 ], Space( 60 ) ) }
   else
      if oRec:aData[ nAt, 2 ] == nil
         DEFAULT bGet := { |x| If( x == nil, IfNil( oRec:aData[ nAt, 2 ], Space( 60 ) ), ;
                           oRec:aData[ nAt, 2 ] := If( Empty( x ), nil, x ) ) }
      else
         DEFAULT bGet := bSETGET( oRec:aData[ nAt, 2 ] )
      endif
      bValid   := aLine[ 5 ]
   endif

   if ValType( aLine[ 4 ] ) == 'C' 
		if Len( aLine[ 4 ] ) > 1
      	cPic     := aLine[ 4 ]
      endif
   elseif ValType( Eval( bGet ) ) == 'N'
      cPic     := GetNumPict( Eval( bGet ) )
   endif
   if lMemo
      nGetWidth   := 360
   else
      if cType == 'C'
         cVal     := Replicate( 'W', Len( Eval( bGet ) ) )
         lPassword:= ( iif(ValType( aLine[ 4 ] ) == 'L' , aLine[ 4 ] == .t.,.f. ) ) .or. ;
                     "PASSWORD" $ Upper( ::aPrompts[ nAt ] ) .or. ;
                     "PASSWORD" $ Upper( aLine[ 1 ] )
      else
         cVal     := Replicate( '9', Len( Transform( Eval( bGet ), cPic ) ) )
      endif
      if lCombo
         if ValType( aLine[ 4 ][ 1 ] ) == 'A'
            AEval( aLine[ 4 ], { |a| If( Len( a[ 2 ] ) > Len( cVal ), cVal := a[ 2 ], nil ) } )
         else
            AEval( aLine[ 4 ], { |c| If( Len( c ) > Len( cVal ), cVal := c, nil ) } )
         endif
         cVal     := Replicate( 'W', Len( cVal ) )
      endif
		if lSayDescription
			nGetWidth   := GetTextWidth( hDc, cVal, oPanel:oFont:hFont ) 
		else 
			nGetWidth   := GetTextWidth( hDc, cVal, oPanel:oFont:hFont ) + 20 + If( lCombo, 30, 0 )
			nGetWidth   := Min( 500, nGetWidth )
		endif
   endif
	*
	IF upper(left(::aPrompts[ nAt ],4)) == "SAY:"
		return nil
	ENDIF 
	*
   if lMemo
      @ nRow, 20 SAY ::aPrompts[ nAt ] SIZE  nSayWidth,ROWHT PIXEL OF oPanel FONT oSayFont TRANSPARENT
   else
      @ nRow, 20 SAY ::aPrompts[ nAt ] + " :" SIZE  nSayWidth,ROWHT PIXEL OF oPanel FONT oSayFont RIGHT TRANSPARENT
   endif

   if lMemo
      oGet := ;
      TMultiGet():New( nRow + 22 + 4, 20, bGet, oPanel, 500, 4 * 22 + 3 * 4, oFixed, ;
         .F.,nil,nil,nil, .T.,nil, .t.,nil, .F., .F., lReadOnly, bValid, nil, .F., nil, nil )

   elseif lCheck
      oGet   := ;
      TCheckBox():new( nRow, nCol, "", bGet, oPanel, ROWHT,ROWHT, nil, ;
         bValid, oPanel:oFont, nil, nil, nil, ;
         .f., .t., nil, .t., nil )

   elseif lCombo
      if ValType( aLine[ 4 ][ 1 ] ) == 'A'
         oGet := ;
         TDBCombo():New( nRow, nCol, bGet, nil, nGetWidth, If( IsAppThemed(), ROWHT, 300 ), oPanel, nil, ;
             bValid, nil, nil, nil,;
             .t., oPanel:oFont, nil, .t., nil,;
             .f., nil, nil, ;
             aLine[ 4 ], '1', '2', NIL ) //<aList> )
      else
         oGet   := ;
         TComboBox():New( nRow, nCol, bGet, aLine[ 4 ], nGetWidth, If( IsAppThemed(), ROWHT, 300 ), ;
            oPanel, nil, ;
            bValid, nil, nil, nil, ;
            .T., oPanel:oFont, nil, .T., nil, ;
            .F., nil, nil, nil, ;
            nil, nil )
      endif
   else
      if Left( ::aPrompts[ nAT ], 4 ) == "nClr" .or. ;
         Left( ::aData[ nAt, 1 ], 4 ) == "nClr"
         bAction   := { |o| BtnChooseColor( o ) }
      endif
      if lSayDescription
			if Valtype(::aData[ nAt + 1 ][5]) == "B"
				bAction := ::aData[ nAt + 1 ][5]
				cBMP    := StrTran(Alltrim(Upper(::aPrompts[ nAt + 1 ])),'SAY:')
			endif	
      endif
		oGet   := ;
      TGet():New( nRow, nCol, bGet, oPanel, nGetWidth,ROWHT, cPic, ;
               bValid, nil, nil, oPanel:oFont, .F., ;
               nil, .T., nil, .T., nil, ;
               .F., VALTYPE(EVAL(bGet)) $ "DNT", ;
               nil, lReadOnly, lPassword, .f., nil, nil, ;
               nil, nil, nil, nil, bAction,cBMP )
      oGet:nClrTextDis  := CLR_BLACK
      oGet:nClrPaneDis  := GetSysColor( COLOR_BTNFACE )
      oGet:lDiscolors   := .f.
      if lPassWord
         oGet:WinStyle( ES_PASSWORD, .t. )
      endif
      oGet:WinStyle( ES_AUTOHSCROLL, .t. )
		if lSayDescription
		   *
			aSay := ::aData[ nAt + 1 ]
			bSay := aSay[2]
			eSay := eval(aSay[2])
			*
			cSay     := Replicate( 'W', Len( Eval( bSay ) ) )
			nSayGetWidth   := oPanel:nWidth
			nSayGetWidth   := Min( 500, nSayGetWidth )
			*
			oSay := TSay():New( nRow, nGetWidth + nCol + 5, bSay,;
             oPanel, nil, oSayFont, .f., .f., .f.,;
             .t., nil, nil, nSayGetWidth,ROWHT,;
             .f., .t., .f., .f., .f., .t.,;
             .t., aSay[1] )
			*
			oGet:Cargo := oSay
			*
		endif 
		*
   endif

  	nRow     += If( lMemo, 5, 1 ) * ( ROWHT + ROWGAP )

return nil

//----------------------------------------------------------------------------//

METHOD DlgButtons( oDlg, oPanel, lNavigate, lReadOnly, nGets ) CLASS TDataRow

   local oRec           := Self
   local oFont
   local nRow, nCol, oBtn
   local lCanNavigate   := !( ::cSrcType $ "ARR,HSH" )  .and. ! Empty( ::bGoTop )
   local lDataSource    := !( ::cSrcType $ "ARR,HSH" )
   local aBmp           := MakeBmpArray( ::cBmpList )

   nRow        := oPanel:nBottom + ROWHT
   nCol        := oPanel:nRight - 32

   if !lDataSource
      aBmp[ 1, 3 ]   :=  Chr(0xFC)
   endif

   oFont := TFont():New( "Wingdings", 0, -22, .f., .f., 0, 0, 400, .f., .f., .f., 2,3, 2, 1,, 18 )
   @ nRow, nCol BTNBMP oBtn FILE aBmp[ 1, 1 ] RESOURCE aBmp[ 1, 2 ] PROMPT aBmp[ 1, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
      TOOLTIP "Close" ACTION ( lExit := .t., oDlg:End() )
   if ! lDataSource
      oBtn:SetColor( CLR_GREEN, oBtn:nClrPane )
   endif
   if ! lReadOnly
      if lDataSource
         nCol  -= 36
         @ nRow, nCol BTNBMP FILE aBmp[ 2, 1 ] RESOURCE aBmp[ 2, 2 ] PROMPT aBmp[ 2, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
            TOOLTIP "Save" ACTION ( oRec:Save( .t. ), oPanel:Update(), oPanel:SetFocus() ) ;
            WHEN oRec:Modified() .or. nGets < 2
      else
         nCol  -= 36
         @ nRow, nCol BTNBMP oBtn PROMPT Chr( 0xFB ) SIZE 32,32 PIXEL OF oDlg FONT oFont ;
            TOOLTIP "Cancel" ACTION ( oRec:Undo(), lExit := .f., oDlg:End() )
         oBtn:SetColor( CLR_HRED, oBtn:nClrPane )
         oBtn:lCancel   := .t.
      endif
   endif

   RELEASE FONT oFont
   oFont := TFont():New( "Wingdings 3", 0, -22, .f., .f., 0, 0, 400, .f., .f., .f., 2,3, 2, 1,, 18 )
   if ! lReadOnly
      nCol  -= 36
      @ nRow, nCol BTNBMP oBtn FILE aBmp[ 3, 1 ] RESOURCE aBmp[ 3, 2 ] PROMPT aBmp[ 3, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "UnDo" ACTION ( oRec:Undo(), oPanel:Update(), oPanel:SetFocus() ) WHEN oRec:Modified() .or. nGets < 2
         oBtn:lCancel   := .t.
      if lDataSource
         nCol  -= 36
         @ nRow, nCol BTNBMP oBtn FILE aBmp[ 4, 1 ] RESOURCE aBmp[ 4, 2 ] PROMPT aBmp[ 4, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
            TOOLTIP "Refresh" ACTION ( oRec:Load( oRec:RecNo == 0 ), oPanel:Update(), oPanel:SetFocus() )
         oBtn:lCancel   := .t.
      endif
   endif
   RELEASE FONT oFont

   if lNavigate .and. lCanNavigate
      oFont := TFont():New( "Wingdings 3", 0, -22, .f., .f., 0, 0, 400, .f., .f., .f., 2,3, 2, 1,, 18 )
      nCol  := oPanel:nLeft
      @ nRow, nCol BTNBMP FILE aBmp[ 5, 1 ] RESOURCE aBmp[ 5, 2 ] PROMPT aBmp[ 5, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "GoTop" ACTION ( oRec:GoTop(), oPanel:Update() ) ;
         WHEN ( oRec:CanGoUp() )
      nCol  += 36
      @ nRow, nCol BTNBMP FILE aBmp[ 6, 1 ] RESOURCE aBmp[ 6, 2 ] PROMPT aBmp[ 6, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "GoUp"   ACTION ( oRec:GoUp(), oPanel:Update() ) ;
         WHEN ( oRec:CanGoUp() )
      nCol  += 36
      @ nRow, nCol BTNBMP FILE aBmp[ 7, 1 ] RESOURCE aBmp[ 7, 2 ] PROMPT aBmp[ 7, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "GoDown"   ACTION ( oRec:GoDown(), oPanel:Update() ) ;
         WHEN ( oRec:CanGoDn() )
      nCol  += 36
      @ nRow, nCol BTNBMP FILE aBmp[ 8, 1 ] RESOURCE aBmp[ 8, 2 ] PROMPT aBmp[ 8, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "GoBottom"   ACTION ( oRec:GoBottom(), oPanel:Update() ) ;
         WHEN ( oRec:CanGoDn() )
      RELEASE FONT oFont

      oFont := TFont():New( "Wingdings 2", 0, -22, .f., .f., 0, 0, 400, .f., .f., .f., 2,3, 2, 1,, 18 )
      nCol  += 38
      @ nRow, nCol BTNBMP FILE aBmp[ 9, 1 ] RESOURCE aBmp[ 9, 2 ] PROMPT aBmp[ 9, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "AddNew" ACTION ( oRec:Load( .t. ), oPanel:Update() ) ;
         WHEN oRec:RecNo > 0
/*
      nCol  += 36
      @ nRow, nCol BTNBMP FILE aBmp[ 10, 1 ] RESOURCE aBmp[ 10, 2 ] PROMPT aBmp[ 10, 3 ] SIZE 32,32 PIXEL OF oDlg FONT oFont ;
         TOOLTIP "Delete"
*/
      RELEASE FONT oFont
   endif

return nil

//----------------------------------------------------------------------------//

METHOD EqualVal( x, y ) CLASS TDataRow

   local c, lEq := .f.

   x  := XEval( x )

   if Empty( x ) .and. y == nil
      return .t.
   endif

   if ( c := ValType( x ) ) == ValType( y )
      if c == 'C'
         lEq   := ( Trim( x ) == Trim( y ) )
      else
         lEq   := ( x == y )
      endif
   endif

return lEq

//----------------------------------------------------------------------------//

METHOD nomessage(...) CLASS TDataRow

   local cMsg     := __GetMessage()
   local lAssign  := Left( cMsg, 1 ) == '_'
   local nPos, uVal, e

   if lAssign
      cMsg := SubStr( cMsg, 2 )
   endif
   nPos  := ::FieldPos( cMsg )
   if nPos > 0
      if lAssign
         uVal                 := HB_AParams()[ 1 ]
/*
         if ! ::lTypeCheck .or. ( ValType( uVal ) == ValType( ::aData[ nPos, 2 ] ) )
            ::aData[ nPos, 2 ]   := uVal
         else
            return ::error(  HB_LangErrMsg( 33 ), ::className(), cMsg, 33, { uVal } )
         endif
*/
         return ::FieldPut( nPos, uVal )
      endif
      return ::FieldGet( nPos )  //::aData[ nPos, 2 ]
   endif
   _ClsSetError( _GenError( If( lAssign, 1005, 1004 ), ::ClassName(), cMsg ) )

return nil

//----------------------------------------------------------------------------//

PROCEDURE Destroy CLASS TDataRow

   ::End()

return

//----------------------------------------------------------------------------//

//----------------------------------------------------------------------------//

static function XbrAddNewRow( oBrw )

   local lAdded   := .f.
   local aNew

   SWITCH oBrw:nDataType
   case DATATYPE_RDD
      REPEAT
         ( oBrw:cAlias )->( DBAPPEND() )
      UNTIL ! NetErr()
      lAdded   := .t.
      exit
   case DATATYPE_ADO
      oBrw:oRs:AddNew()
      lAdded   := .t.
      exit
   case DATATYPE_ODBF
      oBrw:oDbf:Append()
      lAdded   := .t.
      exit
   case DATATYPE_ARRAY
      if oBrw:nLen > 0
         aNew     := AClone( oBrw:aArrayData[ oBrw:nLen ] )
         AEval( aNew, { |u,i| aNew[ i ] := BLANK( u ) } )
         AAdd( oBrw:aArrayData, aNew )
         oBrw:nLen++
         oBrw:nArrayAt  := Len( oBrw:aArrayData )
         oBrw:GoBottom()
         lAdded   := .t.
      endif
      exit
   END

return lAdded

//----------------------------------------------------------------------------//

static function XbrSaveNewRow( oBrw )

   local lSaved   := .f.

   SWITCH oBrw:nDataType
   case DATATYPE_RDD
      ( oBrw:cAlias )->( DBCOMMIT() )
      lSaved   := .t.
      exit
   case DATATYPE_ADO
      TRY
         oBrw:oRs:Update()
         lSaved   := .t.
      CATCH
         oBrw:oRs:CancelUpdate()
         FW_ShowAdoError( oBrw:oRs:ActiveConnection )
      END
      exit
   case DATATYPE_ODBF
      oBrw:oDbf:Save()
      lSaved   := .t.
      exit
   case DATATYPE_ARRAY
      lSaved   := .t.
      exit
   END

return lSaved

//----------------------------------------------------------------------------//

static function TrimList( cList )

   cList    := AllTrim( cList )

   do while ", " $ cList
      cList    := StrTran( cList, ", ", "," )
   enddo
   do while " ," $ cList
      cList    := StrTran( cList, " ,", "," )
   enddo

return cList

//----------------------------------------------------------------------------//

static function GetNumPict( n ); return NumPict( 11, GetDec( n ) )

//----------------------------------------------------------------------------//

static function GetDec( n )

   local nDec  := 0
   local c     := cValToChar( n )

   if '.' $ c
      nDec  := Len( AfterAtNum( '.', c ) )
   endif

return nDec

//----------------------------------------------------------------------------//

function MakeBmpArray( cList )

   local aBmp  := {}
   local aPrompts := { "close", "save", "undo", "redo", "top", "up", "down", "bottom", "new", "delete" }
   local cChar := Chr(0x30)+Chr(0x3C)+Chr(0x51)+Chr(0x50)+Chr(0x76)+Chr(0xD1)+Chr(0xD2)+Chr(0x77)+Chr(0x2F)+Chr(0x25)
//   local cChar := Chr(0xFC)+Chr(0x3C)+Chr(0x51)+Chr(0x50)+Chr(0x76)+Chr(0xD1)+Chr(0xD2)+Chr(0x77)+Chr(0x2F)+Chr(0x25)
   local n, cBmp

   DEFAULT cList  := ""
   for n := 1 to Len( aPrompts )
      cBmp  := ExtractBmp( aPrompts[ n ], cList )
      if Empty( cBmp )
         AAdd( aBmp, { nil, nil, SubStr( cChar, n, 1 ) } )
      else
         AAdd( aBmp, { If( '.' $ cBmp, cBmp, nil ), If( '.' $ cBmp, nil, cBmp ), nil } )
      endif
   next

return aBmp

//----------------------------------------------------------------------------//

function ExtractBmp( cPrompt, cList )

   local cBmp
   local nAt

   if ( nAt := At( Upper( cPrompt ) + '=', Upper( cList ) ) ) > 0
      cList    := StrTran( SubStr( cList, nAt + Len( cPrompt ) + 1 ), ';', ',' )
      if ( nAt := At( ',', cList ) ) > 0
         cList := Left( cList, nAt - 1 )
      endif
      cBmp     := AllTrim( cList )
   endif

return cBmp

//----------------------------------------------------------------------------//

function BtnChooseColor(...)

   local aParams  := HB_AParams()
   local uREt, nClr
   local oGet, oCol

   if Len( aParams ) > 0
      if ValType( aParams[ 1 ] ) == 'O' .and. aParams[ 1 ]:IsKindOf( "TGET" )
         oGet     := aParams[ 1 ]
      elseif Len( aParams ) >= 4 .and. ValType( aParams[ 3 ] ) == 'O' .and. ;
         aParams[ 3 ]:IsKindOf( "TXBRWCOLUMN" )
         oCol     := aParams[ 3 ]
      elseif ValType( aParams[ 1 ] ) == 'N'
         nClr     := aParams[ 1 ]
      endif
   endif

   uRet     := ChooseColor( nClr )
   if uRet == 0
      uRet  := nil
   else
      if oGet != nil
         if ValType( oGet:oGet:VarGet() ) == 'C'
            oGet:cText     := cClrToCode( uRet )
         else
            oGet:cText     := uRet
         endif
      elseif oCol != nil
         if ValType( oCol:Value ) == 'C'
            uRet     := cClrToCode( uRet )
         endif
      elseif nClr == nil
         uRet     := cClrToCode( uRet )
      endif
   endif

return uRet
		
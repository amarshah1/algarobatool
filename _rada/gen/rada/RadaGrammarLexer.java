// Generated from /home/hung/Dropbox/work/svn/svn_trusted_verification_rada/rada/src/rada/RadaGrammar.g4 by ANTLR 4.1

package rada;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class RadaGrammarLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		UNDERSCORE=1, LPAREN=2, RPAREN=3, AS=4, LET=5, FORALL=6, EXISTS=7, EXCLIMATIONPT=8, 
		SETLOGIC=9, SETOPTION=10, SETINFO=11, DECLARESORT=12, DEFINESORT=13, DECLAREFUN=14, 
		DEFINEFUN=15, PUSH=16, POP=17, ASSERT=18, CHECKSAT=19, GETASSERT=20, GETPROOF=21, 
		GETUNSATCORE=22, GETVALUE=23, GETASSIGN=24, GETOPTION=25, GETINFO=26, 
		EXIT=27, DECLAREDTYPES=28, DEFINECATA=29, POSTCOND=30, WS=31, COMMENT=32, 
		HEXADECIMAL=33, BINARY=34, ASCIIWOR=35, KEYWORD=36, SYMBOL=37, STRINGLIT=38, 
		NUMERAL=39, DECIMAL=40;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'_'", "'('", "')'", "'as'", "'let'", "'forall'", "'exists'", "'!'", "'set-logic'", 
		"'set-option'", "'set-info'", "'declare-sort'", "'define-sort'", "'declare-fun'", 
		"'define-fun'", "'push'", "'pop'", "'assert'", "'check-sat'", "'get-assertions'", 
		"'get-proof'", "'get-unsat-core'", "'get-value'", "'get-assignment'", 
		"'get-option'", "'get-info'", "'exit'", "'declare-datatypes'", "'define-catamorphism'", 
		"':post-cond'", "WS", "COMMENT", "HEXADECIMAL", "BINARY", "ASCIIWOR", 
		"KEYWORD", "SYMBOL", "STRINGLIT", "NUMERAL", "DECIMAL"
	};
	public static final String[] ruleNames = {
		"UNDERSCORE", "LPAREN", "RPAREN", "AS", "LET", "FORALL", "EXISTS", "EXCLIMATIONPT", 
		"SETLOGIC", "SETOPTION", "SETINFO", "DECLARESORT", "DEFINESORT", "DECLAREFUN", 
		"DEFINEFUN", "PUSH", "POP", "ASSERT", "CHECKSAT", "GETASSERT", "GETPROOF", 
		"GETUNSATCORE", "GETVALUE", "GETASSIGN", "GETOPTION", "GETINFO", "EXIT", 
		"DECLAREDTYPES", "DEFINECATA", "POSTCOND", "WS", "COMMENT", "HEXADECIMAL", 
		"BINARY", "ASCIIWOR", "KEYWORD", "SYMBOL_CHAR_NOUNDERSCORE_NOATTRIBUTE", 
		"SYMBOL_CHAR", "SYMBOL", "STRINGLIT", "ESCAPE", "NUMERAL", "DECIMAL", 
		"ALPHA", "PDIGIT", "DIGIT"
	};


	public RadaGrammarLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "RadaGrammar.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 30: WS_action((RuleContext)_localctx, actionIndex); break;

		case 31: COMMENT_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: _channel = HIDDEN;  break;
		}
	}
	private void COMMENT_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 1: _channel = HIDDEN;  break;
		}
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\2*\u01e5\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\5\3\6\3\6\3\6"+
		"\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3"+
		"\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3"+
		"\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3"+
		"\16\3\16\3\16\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3"+
		"\17\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3\20\3"+
		"\20\3\20\3\21\3\21\3\21\3\21\3\21\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3"+
		"\23\3\23\3\23\3\23\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3"+
		"\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3"+
		"\25\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\27\3\27\3\27\3"+
		"\27\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\30\3\30\3"+
		"\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31\3\31\3"+
		"\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\31\3\32\3\32\3\32\3\32\3\32\3"+
		"\32\3\32\3\32\3\32\3\32\3\32\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3"+
		"\33\3\34\3\34\3\34\3\34\3\34\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3"+
		"\35\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\36\3\36\3\36\3\36\3"+
		"\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3"+
		"\36\3\36\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3 \6 "+
		"\u0171\n \r \16 \u0172\3 \3 \3!\3!\7!\u0179\n!\f!\16!\u017c\13!\3!\3!"+
		"\3\"\3\"\3\"\3\"\3\"\6\"\u0185\n\"\r\"\16\"\u0186\3#\3#\3#\3#\6#\u018d"+
		"\n#\r#\16#\u018e\3$\3$\7$\u0193\n$\f$\16$\u0196\13$\3$\3$\3%\3%\3%\3%"+
		"\6%\u019e\n%\r%\16%\u019f\3&\3&\3\'\3\'\5\'\u01a6\n\'\3(\3(\5(\u01aa\n"+
		"(\3(\3(\3(\6(\u01af\n(\r(\16(\u01b0\3(\3(\5(\u01b5\n(\3)\3)\3)\7)\u01ba"+
		"\n)\f)\16)\u01bd\13)\3)\3)\3*\3*\3*\3+\3+\3+\7+\u01c7\n+\f+\16+\u01ca"+
		"\13+\5+\u01cc\n+\3,\3,\3,\7,\u01d1\n,\f,\16,\u01d4\13,\5,\u01d6\n,\3,"+
		"\3,\6,\u01da\n,\r,\16,\u01db\3-\3-\3.\3.\3/\3/\5/\u01e4\n/\2\60\3\3\1"+
		"\5\4\1\7\5\1\t\6\1\13\7\1\r\b\1\17\t\1\21\n\1\23\13\1\25\f\1\27\r\1\31"+
		"\16\1\33\17\1\35\20\1\37\21\1!\22\1#\23\1%\24\1\'\25\1)\26\1+\27\1-\30"+
		"\1/\31\1\61\32\1\63\33\1\65\34\1\67\35\19\36\1;\37\1= \1?!\2A\"\3C#\1"+
		"E$\1G%\1I&\1K\2\1M\2\1O\'\1Q(\1S\2\1U)\1W*\1Y\2\1[\2\1]\2\1\3\2\t\5\2"+
		"\13\f\16\17\"\"\4\2\f\f\17\17\4\2^^~~\b\2&(,-/\61>B``\u0080\u0080\4\2"+
		"##aa\4\2$$^^\4\2C\\c|\u01f6\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3"+
		"\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2"+
		"\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37"+
		"\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3"+
		"\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3\2\2\2\2\65\3\2\2\2\2"+
		"\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2\2?\3\2\2\2\2A\3\2\2\2\2C"+
		"\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2U\3\2"+
		"\2\2\2W\3\2\2\2\3_\3\2\2\2\5a\3\2\2\2\7c\3\2\2\2\te\3\2\2\2\13h\3\2\2"+
		"\2\rl\3\2\2\2\17s\3\2\2\2\21z\3\2\2\2\23|\3\2\2\2\25\u0086\3\2\2\2\27"+
		"\u0091\3\2\2\2\31\u009a\3\2\2\2\33\u00a7\3\2\2\2\35\u00b3\3\2\2\2\37\u00bf"+
		"\3\2\2\2!\u00ca\3\2\2\2#\u00cf\3\2\2\2%\u00d3\3\2\2\2\'\u00da\3\2\2\2"+
		")\u00e4\3\2\2\2+\u00f3\3\2\2\2-\u00fd\3\2\2\2/\u010c\3\2\2\2\61\u0116"+
		"\3\2\2\2\63\u0125\3\2\2\2\65\u0130\3\2\2\2\67\u0139\3\2\2\29\u013e\3\2"+
		"\2\2;\u0150\3\2\2\2=\u0164\3\2\2\2?\u0170\3\2\2\2A\u0176\3\2\2\2C\u017f"+
		"\3\2\2\2E\u0188\3\2\2\2G\u0190\3\2\2\2I\u0199\3\2\2\2K\u01a1\3\2\2\2M"+
		"\u01a5\3\2\2\2O\u01b4\3\2\2\2Q\u01b6\3\2\2\2S\u01c0\3\2\2\2U\u01cb\3\2"+
		"\2\2W\u01d5\3\2\2\2Y\u01dd\3\2\2\2[\u01df\3\2\2\2]\u01e3\3\2\2\2_`\7a"+
		"\2\2`\4\3\2\2\2ab\7*\2\2b\6\3\2\2\2cd\7+\2\2d\b\3\2\2\2ef\7c\2\2fg\7u"+
		"\2\2g\n\3\2\2\2hi\7n\2\2ij\7g\2\2jk\7v\2\2k\f\3\2\2\2lm\7h\2\2mn\7q\2"+
		"\2no\7t\2\2op\7c\2\2pq\7n\2\2qr\7n\2\2r\16\3\2\2\2st\7g\2\2tu\7z\2\2u"+
		"v\7k\2\2vw\7u\2\2wx\7v\2\2xy\7u\2\2y\20\3\2\2\2z{\7#\2\2{\22\3\2\2\2|"+
		"}\7u\2\2}~\7g\2\2~\177\7v\2\2\177\u0080\7/\2\2\u0080\u0081\7n\2\2\u0081"+
		"\u0082\7q\2\2\u0082\u0083\7i\2\2\u0083\u0084\7k\2\2\u0084\u0085\7e\2\2"+
		"\u0085\24\3\2\2\2\u0086\u0087\7u\2\2\u0087\u0088\7g\2\2\u0088\u0089\7"+
		"v\2\2\u0089\u008a\7/\2\2\u008a\u008b\7q\2\2\u008b\u008c\7r\2\2\u008c\u008d"+
		"\7v\2\2\u008d\u008e\7k\2\2\u008e\u008f\7q\2\2\u008f\u0090\7p\2\2\u0090"+
		"\26\3\2\2\2\u0091\u0092\7u\2\2\u0092\u0093\7g\2\2\u0093\u0094\7v\2\2\u0094"+
		"\u0095\7/\2\2\u0095\u0096\7k\2\2\u0096\u0097\7p\2\2\u0097\u0098\7h\2\2"+
		"\u0098\u0099\7q\2\2\u0099\30\3\2\2\2\u009a\u009b\7f\2\2\u009b\u009c\7"+
		"g\2\2\u009c\u009d\7e\2\2\u009d\u009e\7n\2\2\u009e\u009f\7c\2\2\u009f\u00a0"+
		"\7t\2\2\u00a0\u00a1\7g\2\2\u00a1\u00a2\7/\2\2\u00a2\u00a3\7u\2\2\u00a3"+
		"\u00a4\7q\2\2\u00a4\u00a5\7t\2\2\u00a5\u00a6\7v\2\2\u00a6\32\3\2\2\2\u00a7"+
		"\u00a8\7f\2\2\u00a8\u00a9\7g\2\2\u00a9\u00aa\7h\2\2\u00aa\u00ab\7k\2\2"+
		"\u00ab\u00ac\7p\2\2\u00ac\u00ad\7g\2\2\u00ad\u00ae\7/\2\2\u00ae\u00af"+
		"\7u\2\2\u00af\u00b0\7q\2\2\u00b0\u00b1\7t\2\2\u00b1\u00b2\7v\2\2\u00b2"+
		"\34\3\2\2\2\u00b3\u00b4\7f\2\2\u00b4\u00b5\7g\2\2\u00b5\u00b6\7e\2\2\u00b6"+
		"\u00b7\7n\2\2\u00b7\u00b8\7c\2\2\u00b8\u00b9\7t\2\2\u00b9\u00ba\7g\2\2"+
		"\u00ba\u00bb\7/\2\2\u00bb\u00bc\7h\2\2\u00bc\u00bd\7w\2\2\u00bd\u00be"+
		"\7p\2\2\u00be\36\3\2\2\2\u00bf\u00c0\7f\2\2\u00c0\u00c1\7g\2\2\u00c1\u00c2"+
		"\7h\2\2\u00c2\u00c3\7k\2\2\u00c3\u00c4\7p\2\2\u00c4\u00c5\7g\2\2\u00c5"+
		"\u00c6\7/\2\2\u00c6\u00c7\7h\2\2\u00c7\u00c8\7w\2\2\u00c8\u00c9\7p\2\2"+
		"\u00c9 \3\2\2\2\u00ca\u00cb\7r\2\2\u00cb\u00cc\7w\2\2\u00cc\u00cd\7u\2"+
		"\2\u00cd\u00ce\7j\2\2\u00ce\"\3\2\2\2\u00cf\u00d0\7r\2\2\u00d0\u00d1\7"+
		"q\2\2\u00d1\u00d2\7r\2\2\u00d2$\3\2\2\2\u00d3\u00d4\7c\2\2\u00d4\u00d5"+
		"\7u\2\2\u00d5\u00d6\7u\2\2\u00d6\u00d7\7g\2\2\u00d7\u00d8\7t\2\2\u00d8"+
		"\u00d9\7v\2\2\u00d9&\3\2\2\2\u00da\u00db\7e\2\2\u00db\u00dc\7j\2\2\u00dc"+
		"\u00dd\7g\2\2\u00dd\u00de\7e\2\2\u00de\u00df\7m\2\2\u00df\u00e0\7/\2\2"+
		"\u00e0\u00e1\7u\2\2\u00e1\u00e2\7c\2\2\u00e2\u00e3\7v\2\2\u00e3(\3\2\2"+
		"\2\u00e4\u00e5\7i\2\2\u00e5\u00e6\7g\2\2\u00e6\u00e7\7v\2\2\u00e7\u00e8"+
		"\7/\2\2\u00e8\u00e9\7c\2\2\u00e9\u00ea\7u\2\2\u00ea\u00eb\7u\2\2\u00eb"+
		"\u00ec\7g\2\2\u00ec\u00ed\7t\2\2\u00ed\u00ee\7v\2\2\u00ee\u00ef\7k\2\2"+
		"\u00ef\u00f0\7q\2\2\u00f0\u00f1\7p\2\2\u00f1\u00f2\7u\2\2\u00f2*\3\2\2"+
		"\2\u00f3\u00f4\7i\2\2\u00f4\u00f5\7g\2\2\u00f5\u00f6\7v\2\2\u00f6\u00f7"+
		"\7/\2\2\u00f7\u00f8\7r\2\2\u00f8\u00f9\7t\2\2\u00f9\u00fa\7q\2\2\u00fa"+
		"\u00fb\7q\2\2\u00fb\u00fc\7h\2\2\u00fc,\3\2\2\2\u00fd\u00fe\7i\2\2\u00fe"+
		"\u00ff\7g\2\2\u00ff\u0100\7v\2\2\u0100\u0101\7/\2\2\u0101\u0102\7w\2\2"+
		"\u0102\u0103\7p\2\2\u0103\u0104\7u\2\2\u0104\u0105\7c\2\2\u0105\u0106"+
		"\7v\2\2\u0106\u0107\7/\2\2\u0107\u0108\7e\2\2\u0108\u0109\7q\2\2\u0109"+
		"\u010a\7t\2\2\u010a\u010b\7g\2\2\u010b.\3\2\2\2\u010c\u010d\7i\2\2\u010d"+
		"\u010e\7g\2\2\u010e\u010f\7v\2\2\u010f\u0110\7/\2\2\u0110\u0111\7x\2\2"+
		"\u0111\u0112\7c\2\2\u0112\u0113\7n\2\2\u0113\u0114\7w\2\2\u0114\u0115"+
		"\7g\2\2\u0115\60\3\2\2\2\u0116\u0117\7i\2\2\u0117\u0118\7g\2\2\u0118\u0119"+
		"\7v\2\2\u0119\u011a\7/\2\2\u011a\u011b\7c\2\2\u011b\u011c\7u\2\2\u011c"+
		"\u011d\7u\2\2\u011d\u011e\7k\2\2\u011e\u011f\7i\2\2\u011f\u0120\7p\2\2"+
		"\u0120\u0121\7o\2\2\u0121\u0122\7g\2\2\u0122\u0123\7p\2\2\u0123\u0124"+
		"\7v\2\2\u0124\62\3\2\2\2\u0125\u0126\7i\2\2\u0126\u0127\7g\2\2\u0127\u0128"+
		"\7v\2\2\u0128\u0129\7/\2\2\u0129\u012a\7q\2\2\u012a\u012b\7r\2\2\u012b"+
		"\u012c\7v\2\2\u012c\u012d\7k\2\2\u012d\u012e\7q\2\2\u012e\u012f\7p\2\2"+
		"\u012f\64\3\2\2\2\u0130\u0131\7i\2\2\u0131\u0132\7g\2\2\u0132\u0133\7"+
		"v\2\2\u0133\u0134\7/\2\2\u0134\u0135\7k\2\2\u0135\u0136\7p\2\2\u0136\u0137"+
		"\7h\2\2\u0137\u0138\7q\2\2\u0138\66\3\2\2\2\u0139\u013a\7g\2\2\u013a\u013b"+
		"\7z\2\2\u013b\u013c\7k\2\2\u013c\u013d\7v\2\2\u013d8\3\2\2\2\u013e\u013f"+
		"\7f\2\2\u013f\u0140\7g\2\2\u0140\u0141\7e\2\2\u0141\u0142\7n\2\2\u0142"+
		"\u0143\7c\2\2\u0143\u0144\7t\2\2\u0144\u0145\7g\2\2\u0145\u0146\7/\2\2"+
		"\u0146\u0147\7f\2\2\u0147\u0148\7c\2\2\u0148\u0149\7v\2\2\u0149\u014a"+
		"\7c\2\2\u014a\u014b\7v\2\2\u014b\u014c\7{\2\2\u014c\u014d\7r\2\2\u014d"+
		"\u014e\7g\2\2\u014e\u014f\7u\2\2\u014f:\3\2\2\2\u0150\u0151\7f\2\2\u0151"+
		"\u0152\7g\2\2\u0152\u0153\7h\2\2\u0153\u0154\7k\2\2\u0154\u0155\7p\2\2"+
		"\u0155\u0156\7g\2\2\u0156\u0157\7/\2\2\u0157\u0158\7e\2\2\u0158\u0159"+
		"\7c\2\2\u0159\u015a\7v\2\2\u015a\u015b\7c\2\2\u015b\u015c\7o\2\2\u015c"+
		"\u015d\7q\2\2\u015d\u015e\7t\2\2\u015e\u015f\7r\2\2\u015f\u0160\7j\2\2"+
		"\u0160\u0161\7k\2\2\u0161\u0162\7u\2\2\u0162\u0163\7o\2\2\u0163<\3\2\2"+
		"\2\u0164\u0165\7<\2\2\u0165\u0166\7r\2\2\u0166\u0167\7q\2\2\u0167\u0168"+
		"\7u\2\2\u0168\u0169\7v\2\2\u0169\u016a\7/\2\2\u016a\u016b\7e\2\2\u016b"+
		"\u016c\7q\2\2\u016c\u016d\7p\2\2\u016d\u016e\7f\2\2\u016e>\3\2\2\2\u016f"+
		"\u0171\t\2\2\2\u0170\u016f\3\2\2\2\u0171\u0172\3\2\2\2\u0172\u0170\3\2"+
		"\2\2\u0172\u0173\3\2\2\2\u0173\u0174\3\2\2\2\u0174\u0175\b \2\2\u0175"+
		"@\3\2\2\2\u0176\u017a\7=\2\2\u0177\u0179\n\3\2\2\u0178\u0177\3\2\2\2\u0179"+
		"\u017c\3\2\2\2\u017a\u0178\3\2\2\2\u017a\u017b\3\2\2\2\u017b\u017d\3\2"+
		"\2\2\u017c\u017a\3\2\2\2\u017d\u017e\b!\3\2\u017eB\3\2\2\2\u017f\u0180"+
		"\7%\2\2\u0180\u0181\7z\2\2\u0181\u0184\3\2\2\2\u0182\u0185\5Y-\2\u0183"+
		"\u0185\5]/\2\u0184\u0182\3\2\2\2\u0184\u0183\3\2\2\2\u0185\u0186\3\2\2"+
		"\2\u0186\u0184\3\2\2\2\u0186\u0187\3\2\2\2\u0187D\3\2\2\2\u0188\u0189"+
		"\7%\2\2\u0189\u018a\7d\2\2\u018a\u018c\3\2\2\2\u018b\u018d\4\62\63\2\u018c"+
		"\u018b\3\2\2\2\u018d\u018e\3\2\2\2\u018e\u018c\3\2\2\2\u018e\u018f\3\2"+
		"\2\2\u018fF\3\2\2\2\u0190\u0194\7~\2\2\u0191\u0193\n\4\2\2\u0192\u0191"+
		"\3\2\2\2\u0193\u0196\3\2\2\2\u0194\u0192\3\2\2\2\u0194\u0195\3\2\2\2\u0195"+
		"\u0197\3\2\2\2\u0196\u0194\3\2\2\2\u0197\u0198\7~\2\2\u0198H\3\2\2\2\u0199"+
		"\u019d\7<\2\2\u019a\u019e\5Y-\2\u019b\u019e\5]/\2\u019c\u019e\5M\'\2\u019d"+
		"\u019a\3\2\2\2\u019d\u019b\3\2\2\2\u019d\u019c\3\2\2\2\u019e\u019f\3\2"+
		"\2\2\u019f\u019d\3\2\2\2\u019f\u01a0\3\2\2\2\u01a0J\3\2\2\2\u01a1\u01a2"+
		"\t\5\2\2\u01a2L\3\2\2\2\u01a3\u01a6\5K&\2\u01a4\u01a6\t\6\2\2\u01a5\u01a3"+
		"\3\2\2\2\u01a5\u01a4\3\2\2\2\u01a6N\3\2\2\2\u01a7\u01aa\5Y-\2\u01a8\u01aa"+
		"\5M\'\2\u01a9\u01a7\3\2\2\2\u01a9\u01a8\3\2\2\2\u01aa\u01ae\3\2\2\2\u01ab"+
		"\u01af\5Y-\2\u01ac\u01af\5]/\2\u01ad\u01af\5M\'\2\u01ae\u01ab\3\2\2\2"+
		"\u01ae\u01ac\3\2\2\2\u01ae\u01ad\3\2\2\2\u01af\u01b0\3\2\2\2\u01b0\u01ae"+
		"\3\2\2\2\u01b0\u01b1\3\2\2\2\u01b1\u01b5\3\2\2\2\u01b2\u01b5\5Y-\2\u01b3"+
		"\u01b5\5K&\2\u01b4\u01a9\3\2\2\2\u01b4\u01b2\3\2\2\2\u01b4\u01b3\3\2\2"+
		"\2\u01b5P\3\2\2\2\u01b6\u01bb\7$\2\2\u01b7\u01ba\5S*\2\u01b8\u01ba\n\7"+
		"\2\2\u01b9\u01b7\3\2\2\2\u01b9\u01b8\3\2\2\2\u01ba\u01bd\3\2\2\2\u01bb"+
		"\u01b9\3\2\2\2\u01bb\u01bc\3\2\2\2\u01bc\u01be\3\2\2\2\u01bd\u01bb\3\2"+
		"\2\2\u01be\u01bf\7$\2\2\u01bfR\3\2\2\2\u01c0\u01c1\7^\2\2\u01c1\u01c2"+
		"\t\7\2\2\u01c2T\3\2\2\2\u01c3\u01cc\7\62\2\2\u01c4\u01c8\5[.\2\u01c5\u01c7"+
		"\5]/\2\u01c6\u01c5\3\2\2\2\u01c7\u01ca\3\2\2\2\u01c8\u01c6\3\2\2\2\u01c8"+
		"\u01c9\3\2\2\2\u01c9\u01cc\3\2\2\2\u01ca\u01c8\3\2\2\2\u01cb\u01c3\3\2"+
		"\2\2\u01cb\u01c4\3\2\2\2\u01ccV\3\2\2\2\u01cd\u01d6\7\62\2\2\u01ce\u01d2"+
		"\5[.\2\u01cf\u01d1\5]/\2\u01d0\u01cf\3\2\2\2\u01d1\u01d4\3\2\2\2\u01d2"+
		"\u01d0\3\2\2\2\u01d2\u01d3\3\2\2\2\u01d3\u01d6\3\2\2\2\u01d4\u01d2\3\2"+
		"\2\2\u01d5\u01cd\3\2\2\2\u01d5\u01ce\3\2\2\2\u01d6\u01d7\3\2\2\2\u01d7"+
		"\u01d9\7\60\2\2\u01d8\u01da\5]/\2\u01d9\u01d8\3\2\2\2\u01da\u01db\3\2"+
		"\2\2\u01db\u01d9\3\2\2\2\u01db\u01dc\3\2\2\2\u01dcX\3\2\2\2\u01dd\u01de"+
		"\t\b\2\2\u01deZ\3\2\2\2\u01df\u01e0\4\63;\2\u01e0\\\3\2\2\2\u01e1\u01e4"+
		"\7\62\2\2\u01e2\u01e4\5[.\2\u01e3\u01e1\3\2\2\2\u01e3\u01e2\3\2\2\2\u01e4"+
		"^\3\2\2\2\30\2\u0172\u017a\u0184\u0186\u018e\u0194\u019d\u019f\u01a5\u01a9"+
		"\u01ae\u01b0\u01b4\u01b9\u01bb\u01c8\u01cb\u01d2\u01d5\u01db\u01e3";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}
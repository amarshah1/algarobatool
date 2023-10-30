/*
 * This file is part of the RADA prototype.
 * 
 * Copyright (C) 2013 University of Minnesota 
 * See file COPYING in the top-level source directory for licensing information 
 */

package rada;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;


public class UnrollingProcedure {
//  private SolverCaller solverCaller;
  private IncrementalSolverCaller incrementalSolverCaller;
  private String aboveCheckSat;
  private List<String> genCatDefinedFuns;      // Generated define-funs
                                               // for catamorphism bodies
//  private List<String> oldGenAssertCatApps;    // Old generated assertions
//                                               // from catamorphism bodies
  
  private List<String> newGenAssertCatApps;    // New generated assertions
                                               // from catamorphism bodies
  
  private List<String> genAssertControlConds;  // Generated assertions from
                                               // control conditions (to avoid
                                               // uninterpreted functions)   
  Map<String, Catamorphism> catMap;
  List<CatamorphismApp> firstCatApps;
  List<CatamorphismApp> allCatApps;            // Set of all catamorphism apps;
  private List<String> genAssertPostConds;     // Generated assertions for
                                               // post conditions of catamorphisms.
  Set<String> currentRecConds;                 // Current set of control conditions.
  
  private int obligationId;
  
  public UnrollingProcedure(int obligationId) {
    this.obligationId = obligationId;
  }
  
  public void terminateSolver() {
    try {
      incrementalSolverCaller.terminate();
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
  }
   
  public void initialize(String obligationContent, String solverName) throws Exception {
//    solverCaller = new SolverCaller(solverName);
    incrementalSolverCaller = new IncrementalSolverCaller(solverName, obligationId);
    aboveCheckSat = null;
    genCatDefinedFuns = new ArrayList<String>();
//    oldGenAssertCatApps = new ArrayList<String>();
    newGenAssertCatApps = new ArrayList<String>();
    genAssertControlConds = new ArrayList<String>();
    catMap = null;
    firstCatApps = null;
    allCatApps = new ArrayList<CatamorphismApp>();
    genAssertPostConds = new ArrayList<String>();       
        
    // Parse the input file   
    RadaGrammarLexer lexer = new RadaGrammarLexer(
        new ANTLRInputStream(obligationContent));
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    RadaGrammarParser parser = new RadaGrammarParser(tokens);
    ParseTree tree = parser.program();  
    ParseTreeWalker walker = new ParseTreeWalker();
    RadaListener listener = new RadaListener(tokens);
    try {
      walker.walk(listener, tree);
    } catch (Exception e) {
      throw new Exception("Error when parsing the obligation content.");
    }
          
//    // Calculates the position of "check-sat" in the parse tree
//    Token checkSatStartToken = listener.getCheckSatStartToken();
//    if (checkSatStartToken == null) {
//      throw new Exception("There is no check-sat in the input file.");
//    }
//    assert(checkSatStartToken != null);
//
//    // Calculates the position of the last token in the parse tree
//    Token programStopToken = listener.getProgramStopToken();
//    
//    // Ignore everything after check-sat
//    listener.getRewriter().delete(checkSatStartToken, programStopToken);
    
    // Save everything before check-sat to a variable
    aboveCheckSat = listener.getRewriter().getText();
   
    // Extracts the definitions of all catamorphisms from input file.
    catMap = listener.getCatmap();

    // For each catamorphism, we look into its body and extracts all
    // catamorphism applications in the body and the corresponding recCondition
    for (String catName : catMap.keySet()) {
      Catamorphism cat = catMap.get(catName);
      cat.createCatApps(catMap);
    }

    // Extract the list of catamorphism applications in the input assertions 
    firstCatApps = listener.getCatApps();
    
    // Sanity checks
    Catamorphism.sanityCheckCatamorphisms(catMap);
    CatamorphismApp.sanityCheckCatamorphismApps(firstCatApps);
    
    // Generate the set of defined funs for catamorphisms
    genCatDefinedFuns = generateCatDefinedFuns();
  }
  
  // Generates define-funs for catamorphisms
  private List<String> generateCatDefinedFuns() {
    List<String> l = new ArrayList<String>();
    for (Catamorphism catamorphism: catMap.values()) {
      l.add("\n; Generated define-fun for the body of catamorphism " + catamorphism.getName());
      l.add(catamorphism.getGeneratedCatDefineFun());
      l.add(catamorphism.getGeneratedUnrollDefineFun());
    }
    return l;
  }
  
  // Adds the current list of catamorphism applications to the set of all
  // catamorphism applications and also generates the post conditions for
  // all of the applications.
  private void addToAllCatApps(List<CatamorphismApp> currentCatApps) {
    for (CatamorphismApp catApp : currentCatApps) {
      if (!allCatApps.contains(catApp)) {
        allCatApps.add(catApp);
        
        // Generate an assertion for the post-condition of the catApp
        Catamorphism cat = catApp.getCatamorphism();
        String postCond = cat.getPostCond();
        if (postCond != null) {
          String postCondOfCatApp = Util.substitute(
              cat.getFormalArgs(), catApp.getActualTerms(), postCond);
          String assertPostCondOfCatApp = Util.makeAssert(postCondOfCatApp);
          if (!genAssertPostConds.contains(assertPostCondOfCatApp)) { 
            genAssertPostConds.add(assertPostCondOfCatApp);
          }
        }
      }
    }
  }
 
//  /**
//   * Generates the content of an SMT file given the current status of the
//   * unrolling algorithm.
//   * 
//   * Note: If withUnrollings = false, we do not care about withControlConditions.
//   */
//  private String genSmt2(boolean withUnrollings, boolean withControlConditions) {
//    List<String> l = new ArrayList<String>();
//    l.add(aboveCheckSat);
//    l.addAll(genCatDefinedFuns);
//    if (withUnrollings) {
////      l.addAll(oldGenAssertCatApps);
//      l.addAll(newGenAssertCatApps);
//      if (withControlConditions && !genAssertControlConds.isEmpty()) {
//        l.add("\n; Assertions for control conditions");
//        l.addAll(genAssertControlConds);
//      }
//    }
//    if (!genAssertPostConds.isEmpty()) {
//      l.add("\n; Assertions for post-conditions of catamorphisms");
//      l.addAll(genAssertPostConds);
//    }
//    l.add(Util.makeCheckSat());
//    // l.add("(get-model)"); //TODO(hungpt): Remove this line
//    return Util.joinStringsWithNewLineSeparator(l);
//  }
//  
  /**
   * Generates the incremental SMT content given the current status of the
   * unrolling algorithm.
   * 
   * Note: If withUnrollings = false, we do not care about withControlConditions.
   */
  private String genSmt2Incrementally(boolean withUnrollings,
      boolean withControlConditions) {
    List<String> l = new ArrayList<String>();
    if (!withUnrollings) {
      // This block is only executed one time only. It is used to check the
      // satisfiability of the problem without unrolling any catamorphisms
      // (i.e., all catamorphism instances are treated as uninterpreted
      // functions.)
      l.add(aboveCheckSat);
      l.addAll(genCatDefinedFuns);
      l.add(Util.makeCheckSat());
    } else {  // This block is executed as the unrolling decision procedure runs.
      // l.addAll(oldGenAssertCatApps);
      if (withControlConditions) {  // We check sat with control conditions
        // Add assertions for catamorphism applications.
        l.addAll(newGenAssertCatApps);
        
        // Generate post-conditions for catamorphism instances
        if (!genAssertPostConds.isEmpty()) {
          l.add("\n; Assertions for post-conditions of catamorphisms");
          l.addAll(genAssertPostConds);
        }
        
        // Push the control conditions, check-sat, and then pop these control conditions
        l.add(Util.makePush());
        if (!genAssertControlConds.isEmpty()) {
          l.add("\n; Assertions for control conditions");
          l.addAll(genAssertControlConds);
        }
        l.add(Util.makeCheckSat());
        l.add(Util.makePop());
      } else {  // We check sat without control conditions
        // The idea of re-using control conditions is implemented in the line below.
        // Currently we disable it because we have not observed any speed gain by using this trick.
        // l.add(Util.makeAssert(Util.makeOr(currentRecConds)));
        l.add(Util.makeCheckSat()); 
      }
    }
    // l.add("(get-model)"); //TODO(hungpt): Remove this line
    return Util.joinStringsWithNewLineSeparator(l);
  }

  /**
   * Unrolls a list of lowest-level catamorphism applications.
   */
  private List<CatamorphismApp> unrollCatApps(List<CatamorphismApp> currentCatApps) {
    List<CatamorphismApp> newCatApps = new ArrayList<CatamorphismApp>();    
    for (CatamorphismApp catApp : currentCatApps) {
      List<CatamorphismApp> childCatApps = catApp.getChildCatApps();
      for (CatamorphismApp childCatApp : childCatApps) {
        if (!newCatApps.contains(childCatApp)) {
          newCatApps.add(childCatApp);
        }
      }
    }
    return newCatApps;
  }
  
  /**
   * Generates assertions for a list of lowest-level catamorphism applications.
   * 
   * Variables: 
   *   oldGenAssertCatApps   : Assertions for old catamorphism applications
   *   newGenAssertCatApps   : Assertions for newly created catamorphism
   *                           applications at the lowest level.                       
   *   genAssertControlConds : Assertions for control conditions.
   */
  private void generateAssertions(List<CatamorphismApp> currentCatApps) {       
//    oldGenAssertCatApps.addAll(newGenAssertCatApps);
    newGenAssertCatApps.clear();
    genAssertControlConds.clear();
    currentRecConds = new TreeSet<String>();
    for (CatamorphismApp newCatApp : currentCatApps) {
      //newGenAssertCatApps.add(newCatApp.createFullAssertCatApp());
      newGenAssertCatApps.add(newCatApp.createFullAssertCatAppsUsingGeneratedUnrollDefineFun());
      if (newCatApp.getCatamorphism().getRecCondition() != null) {
        String recCondition = Util.substitute(
            newCatApp.getCatamorphism().getFormalArgs(), 
            newCatApp.getActualTerms(),
            newCatApp.getCatamorphism().getRecCondition());
        if (!currentRecConds.contains(recCondition)) {
          currentRecConds.add(recCondition);
          String controlCondition = Util.makeNot(recCondition);
          String assertControlCondition = Util.makeAssert(controlCondition);
          genAssertControlConds.add(assertControlCondition);
        }
      }
    }
  }
  
  /**
   * Checks the satisfiability of a file using a solver.
   */  
  public RadaResult checkSat(String obligationContent, SolverParameters parms) {
    RadaResult radaResult = new RadaResult();
    try {
      initialize(obligationContent, parms.solverName);
    } catch (Exception e) {
      radaResult.setResultTypeAndMessage(ResultType.ERROR, e.getMessage());
      return radaResult;
    }
 
    List<CatamorphismApp> currentCatApps = firstCatApps;
    addToAllCatApps(currentCatApps);
    try {
      // First, try to check sat without unrolling catamorphisms.
      //   + If we get UNSAT, we are done. Result is UNSAT.
      //   + If we get SAT, we start the unrolling process.      
      SolverResult firstResult = 
//        solverCaller.checkSat(genSmt2(false, false), parms);
          incrementalSolverCaller.appendContent(genSmt2Incrementally(false, false), parms);
      radaResult.addSolverResult(firstResult);
      if (firstResult.getType() == ResultType.ERROR) {
        radaResult.setResultTypeAndMessage(ResultType.ERROR, 
                                           "Error when execute solver: " + firstResult.getRawResult());
        return radaResult;
      } else if (firstResult.getType() == ResultType.UNSAT) {
        radaResult.setResultType(ResultType.UNSAT);
        return radaResult;
      } else if (firstResult.getType() == ResultType.SAT ||
                 firstResult.getType() == ResultType.UNKNOWN) {  // If UNKNOWN immediately, give it a chance to unroll
        // Start unrolling.
        while (parms.maxDepth < 0 || radaResult.getNumUnrollings() < parms.maxDepth) {
          // System.out.println("Current number of unrollings: " + radaResult.getNumUnrollings());
          addToAllCatApps(unrollCatApps(currentCatApps));
          radaResult.incNumUnrollings();
          generateAssertions(currentCatApps);
          
          // For each unrolling step:
          // First, we check sat with control conditions.
          //   + If we get SAT, we are done. Result is SAT.
          //   + If we get UNSAT, we check sat without control conditions
          //     - If we get UNSAT, we are done. Result is UNSAT
          //     - If we get SAT, we are not. We need to unroll further.
          SolverResult resultWithControlCondition =
//            solverCaller.checkSat(genSmt2(true, true), parms);
              incrementalSolverCaller.appendContent(genSmt2Incrementally(true, true), parms);
          radaResult.addSolverResult(resultWithControlCondition);

          if (resultWithControlCondition.getType() == ResultType.UNKNOWN) {          
            break;
          } else if (resultWithControlCondition.getType() == ResultType.ERROR) {
            radaResult.setResultTypeAndMessage(ResultType.ERROR,
                "Error when execute solver: " + resultWithControlCondition.getRawResult());
            return radaResult;
          } else if (resultWithControlCondition.getType() == ResultType.SAT) {
            radaResult.setResultType(ResultType.SAT);          
            return radaResult;
          } else {  // resultWithControlCondition.getType() == ResultType.UNSAT            
            SolverResult resultWithoutControlCondition = 
//              solverCaller.checkSat(genSmt2(true, false), parms);
                incrementalSolverCaller.appendContent(genSmt2Incrementally(true, false), parms);
            radaResult.addSolverResult(resultWithoutControlCondition);

//            if (resultWithoutControlCondition.getType() == ResultType.UNKNOWN) {
//              break;
//            } else 
            // Hung: If we get UNKNOWN, we continue the unrolling process.
            if (resultWithoutControlCondition.getType() == ResultType.ERROR) {
              radaResult.setResultTypeAndMessage(ResultType.ERROR,
                  "Error when execute solver: " + resultWithoutControlCondition.getRawResult());
              return radaResult;
            } else if (resultWithoutControlCondition.getType() == ResultType.UNSAT) {
              radaResult.setResultType(ResultType.UNSAT);
              return radaResult;
            } else {
              // resultWithoutControlCondition.getType() == ResultType.SAT ||
              // resultWithoutControlCondition.getType() == ResultType.UNKNOWN
              currentCatApps = unrollCatApps(currentCatApps);
              CatamorphismApp.sanityCheckCatamorphismApps(currentCatApps);
            }
          }
        }
      }
    } catch (Exception e) {
      radaResult.setResultTypeAndMessage(ResultType.ERROR, e.getMessage());
      return radaResult;
    }
    
    // If we have not decided whether the problem is satisfiable or not after
    // the maximum allowed number of unrolls, we return UNKNOWN.
    radaResult.setResultType(ResultType.UNKNOWN);
    return radaResult;
  }
}
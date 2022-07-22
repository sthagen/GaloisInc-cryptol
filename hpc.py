import sys
import os
import os.path
import subprocess

# 
# utilities
# 

def shell(cmd):
  print("> " + cmd)
  process = subprocess.Popen(
    cmd.split(),
    stdout=subprocess.PIPE
  )
  output, error = process.communicate()
  output = output.decode('utf-8')
  if output != "":
    print(output)
  return output

def concat(xss):
  return [ x for xs in xss for x in xs ]

# 
# constants
# 

hpc_tixs_dir = "./hpc_tixs"

hpc_dir = "./dist-newstyle/build/x86_64-osx/ghc-8.10.7/cryptol-2.13.0.99/hpc/vanilla/mix/cryptol-2.13.0.99"

mix_files = "Cryptol.AES Cryptol.Backend.Arch Cryptol.Backend.Concrete Cryptol.Backend.FloatHelpers Cryptol.Backend.Monad Cryptol.Backend.SBV Cryptol.Backend.SeqMap Cryptol.Backend.What4 Cryptol.Backend.WordValue Cryptol.Backend Cryptol.Eval.Concrete Cryptol.Eval.Env Cryptol.Eval.Generic Cryptol.Eval.Prims Cryptol.Eval.Reference Cryptol.Eval.SBV Cryptol.Eval.Type Cryptol.Eval.Value Cryptol.Eval.What4 Cryptol.Eval Cryptol.F2 Cryptol.IR.FreeVars Cryptol.ModuleSystem.Base Cryptol.ModuleSystem.Env Cryptol.ModuleSystem.Exports Cryptol.ModuleSystem.Fingerprint Cryptol.ModuleSystem.InstantiateModule Cryptol.ModuleSystem.Interface Cryptol.ModuleSystem.Monad Cryptol.ModuleSystem.Name Cryptol.ModuleSystem.NamingEnv Cryptol.ModuleSystem.Renamer.Error Cryptol.ModuleSystem.Renamer.Monad Cryptol.ModuleSystem.Renamer Cryptol.ModuleSystem Cryptol.Parser.AST Cryptol.Parser.Layout Cryptol.Parser.LexerUtils Cryptol.Parser.Name Cryptol.Parser.Names Cryptol.Parser.NoInclude Cryptol.Parser.NoPat Cryptol.Parser.ParserUtils Cryptol.Parser.Position Cryptol.Parser.Selector Cryptol.Parser.Token Cryptol.Parser.Unlit Cryptol.Parser.Utils Cryptol.Prelude Cryptol.PrimeEC Cryptol.REPL.Browse Cryptol.REPL.Command Cryptol.REPL.Monad Cryptol.REPL.Trie Cryptol.SHA Cryptol.Symbolic.SBV Cryptol.Symbolic.What4 Cryptol.Symbolic Cryptol.Testing.Random Cryptol.Transform.AddModParams Cryptol.Transform.MonoValues Cryptol.Transform.Specialize Cryptol.TypeCheck.AST Cryptol.TypeCheck.CheckModuleInstance Cryptol.TypeCheck.Default Cryptol.TypeCheck.Error Cryptol.TypeCheck.Infer Cryptol.TypeCheck.InferTypes Cryptol.TypeCheck.Instantiate Cryptol.TypeCheck.Interface Cryptol.TypeCheck.Kind Cryptol.TypeCheck.Monad Cryptol.TypeCheck.PP Cryptol.TypeCheck.Parseable Cryptol.TypeCheck.Sanity Cryptol.TypeCheck.SimpType Cryptol.TypeCheck.SimpleSolver Cryptol.TypeCheck.Solve Cryptol.TypeCheck.Solver.Class Cryptol.TypeCheck.Solver.Improve Cryptol.TypeCheck.Solver.InfNat Cryptol.TypeCheck.Solver.Numeric.Fin Cryptol.TypeCheck.Solver.Numeric.Interval Cryptol.TypeCheck.Solver.Numeric Cryptol.TypeCheck.Solver.SMT Cryptol.TypeCheck.Solver.Selector Cryptol.TypeCheck.Solver.Types Cryptol.TypeCheck.Solver.Utils Cryptol.TypeCheck.Subst Cryptol.TypeCheck.TCon Cryptol.TypeCheck.Type Cryptol.TypeCheck.TypeMap Cryptol.TypeCheck.TypeOf Cryptol.TypeCheck.TypePat Cryptol.TypeCheck.Unify Cryptol.TypeCheck Cryptol.Utils.Debug Cryptol.Utils.Fixity Cryptol.Utils.Ident Cryptol.Utils.Logger Cryptol.Utils.Misc Cryptol.Utils.PP Cryptol.Utils.Panic Cryptol.Utils.Patterns Cryptol.Utils.RecordMap GHC.Num.Compat"

# 
# file manipulation
# 

def to_test_dir(test_dir):
  return f'./tests/{test_dir}'

def to_test_file(test_dir, test_name):
  return f'{to_test_dir(test_dir)}/{test_name}.icry'

def to_test_tix_dir(test_dir):
  return f'{hpc_tixs_dir}/{test_dir}'

def to_test_tix_file(test_dir, test_name):
  return f'{to_test_tix_dir(test_dir)}/{test_name}.tix'

# gets all the (test_dir, test_name) from a test_dir
def get_test_dirs_names(test_dir):
  files = os.listdir(to_test_dir(test_dir))
  return [
    (test_dir, os.path.basename(file)[:-5])
    for file in files
    if os.path.basename(file).endswith(".icry")
  ]

# 
# input/output directories
# 

test_dirs_names = concat([
  get_test_dirs_names("mono-binds"),
  get_test_dirs_names("issues"),
  get_test_dirs_names("parser"),
  get_test_dirs_names("regression"),
  get_test_dirs_names("suiteb"),
  get_test_dirs_names("renamer"),
])

combos_dir, combo_name = "combos", "combo"

# 
# commands
# 

def run_test(test_dir, test_name):
  shell(f'./cry test {to_test_file(test_dir, test_name)}')
  shell(f'mkdir -p {to_test_tix_dir(test_dir)}')
  shell(f'mv {to_test_dir(test_dir)}/cryptol.tix {to_test_tix_file(test_dir, test_name)}')

def report_test(test_dir, test_name):
  shell(f'mkdir -p {to_test_tix_dir(test_dir)}')
  out = shell(f'hpc report --hpcdir={hpc_dir} {to_test_tix_file(test_dir, test_name)} {mix_files}')

def markup_test(test_dir, test_name):
  shell(f'mkdir -p {to_test_tix_dir(test_dir)}')
  out = shell(f'hpc markup --hpcdir={hpc_dir} {to_test_tix_file(test_dir, test_name)} {mix_files}')

# combines via ADD into tests/combos/combo.tix
def sum_test_tixs(test_dirs_names):
  test_tix_files = [
    to_test_tix_file(test_dir, test_name)
    for (test_dir, test_name) in test_dirs_names
  ]
  shell(f'mkdir -p {to_test_tix_dir(combos_dir)}')
  shell(f'hpc sum --union --output={to_test_tix_file(combos_dir, combo_name)} {" ".join(test_tix_files)}')

# 
# main
# 
if __name__ == "__main__":
  if not len(sys.argv) == 2:
    print("hpc.py requires 1 argument: [run|sum|report|markup]")

  cmd = sys.argv[1]

  if cmd == "run":
    for (test_dir, test_name) in test_dirs_names:
      run_test(test_dir, test_name)
  elif cmd == "sum":
    sum_test_tixs(test_dirs_names)
  elif cmd == "markup":
    markup_test(combos_dir, combo_name)
  elif cmd == "report":
    report_test(combos_dir, combo_name)
  else:
    print("[error] hpc.py command not recognized: " + cmd)
package main

import (
	"context"
	"fmt"
	"os"
	"reflect"

	"github.com/google/cel-go/cel"
	"github.com/google/cel-go/checker"
	"github.com/google/cel-go/ext"
	"github.com/google/cel-go/interpreter"

	"google.golang.org/protobuf/types/known/structpb"
	"k8s.io/apimachinery/pkg/util/yaml"
)

func main() {
	args := os.Args
	f, err := os.Open("deployment.json")
	if err != nil {
		panic(err)
	}
	defer f.Close()

	deployment := map[string]any{}
	if err := yaml.NewYAMLOrJSONDecoder(f, 4096).Decode(&deployment); err != nil {
		panic(err)
	}

	celEnvOptions := []cel.EnvOption{
		cel.HomogeneousAggregateLiterals(),
		cel.EagerlyValidateDeclarations(true),
		cel.DefaultUTCTimeZone(true),

		cel.CrossTypeNumericComparisons(true),
		cel.OptionalTypes(),

		cel.ASTValidators(
			cel.ValidateDurationLiterals(),
			cel.ValidateTimestampLiterals(),
			cel.ValidateRegexLiterals(),
			cel.ValidateHomogeneousAggregateLiterals(),
		),

		ext.Strings(ext.StringsVersion(2)),
		ext.Sets(),

		cel.CostEstimatorOptions(checker.PresenceTestHasCost(false)),
	}
	for k := range deployment {
		celEnvOptions = append(celEnvOptions, cel.Variable(k, cel.DynType))
	}
	env, err := cel.NewEnv(celEnvOptions...)
	if err != nil {
		panic(err)
	}
	ast, issues := env.Compile(args[1])
	if issues != nil {
		panic(issues.String())
	}

	celProgramOptions := []cel.ProgramOption{
		cel.EvalOptions(cel.OptOptimize, cel.OptTrackCost),
		cel.CostTrackerOptions(interpreter.PresenceTestHasCost(false)),
	}

	prog, err := env.Program(ast, celProgramOptions...)
	if err != nil {
		panic(err)
	}

	val, _, err := prog.ContextEval(context.Background(), deployment)
	if err != nil {
		panic(err)
	}

	value, err := val.ConvertToNative(reflect.TypeOf(&structpb.Value{}))
	if err != nil {
		panic(err)
	}

	fmt.Println(value.(*structpb.Value).GetBoolValue())
}

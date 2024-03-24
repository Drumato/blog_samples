package main

import (
	"context"
	"fmt"
	"os"
	"os/signal"
	"time"

	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracegrpc"
	"go.opentelemetry.io/otel/propagation"
	"go.opentelemetry.io/otel/sdk/resource"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
	semconv "go.opentelemetry.io/otel/semconv/v1.17.0"
	"go.opentelemetry.io/otel/trace"
)

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	exporter, err := otlptracegrpc.New(ctx)
	if err != nil {
		panic(err)
	}

	tp := sdktrace.NewTracerProvider(
		sdktrace.WithResource(newResource()),
		sdktrace.WithSyncer(exporter),
	)
	defer tp.Shutdown(ctx)
	otel.SetTracerProvider(tp)
	otel.SetTextMapPropagator(propagation.NewCompositeTextMapPropagator(propagation.TraceContext{}, propagation.Baggage{}))

	tracer := otel.Tracer("chapter1")

	count := 0
loop:
	for {
		select {
		case <-ctx.Done():
			break loop
		default:
			_, span := tracer.Start(
				ctx,
				fmt.Sprintf("chapter1.%d", count),
				trace.WithAttributes(
					attribute.KeyValue{
						Key:   attribute.Key("value-update"),
						Value: attribute.StringValue("default"),
					},
					attribute.KeyValue{
						Key:   attribute.Key("value-upsert1"),
						Value: attribute.StringValue("default"),
					},
					attribute.KeyValue{
						Key:   attribute.Key("delete-pattern1"),
						Value: attribute.StringValue("default"),
					},
					attribute.KeyValue{
						Key:   attribute.Key("delete-pattern2"),
						Value: attribute.StringValue("default"),
					},
					attribute.KeyValue{
						Key:   attribute.Key("destination"),
						Value: attribute.StringValue("default"),
					},
					attribute.KeyValue{
						Key:   attribute.Key("from-attribute-1"),
						Value: attribute.StringValue("DRUMATO"),
					},
				),
			)
			span.End()

			count++
			time.Sleep(1 * time.Second)
		}
	}
}

func newResource() *resource.Resource {
	return resource.NewWithAttributes(
		semconv.SchemaURL,
		semconv.ServiceName("chapter1-main"),
	)
}

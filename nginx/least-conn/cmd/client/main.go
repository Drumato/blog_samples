package main

import (
	"context"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"sync"
	"time"
)

var processedCount int64
var processedCountMu sync.Mutex

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	logger := slog.New(slog.NewTextHandler(os.Stdout, nil))
	slog.SetDefault(logger)

	i := 0
	wg := &sync.WaitGroup{}
	ticker := time.NewTicker(1 * time.Second)
	for {
		select {
		case <-ticker.C:
			for {
				if processedCountMu.TryLock() {
					fmt.Println("Current proceesed count:", processedCount)
					processedCount = 0
					processedCountMu.Unlock()
					break
				}
			}
		case <-ctx.Done():
			slog.Info("Received interrupt signal, shutting down")
			wg.Wait()
			return
		default:
			wg.Add(1)
			go func(wg *sync.WaitGroup, count int) {
				defer wg.Done()

				var resp *http.Response
				var err error
				if count%3 == 0 {
					resp, err = http.Get("http://localhost:8081")
				} else if count%3 == 1 {
					resp, err = http.Get("http://localhost:8082")
				} else {
					resp, err = http.Get("http://localhost:8083")
				}

				if err != nil {
					slog.Error("failed to get", slog.String("url", fmt.Sprintf("http://localhost:808%d", count%3+1)), slog.String("error", err.Error()))
					return
				}
				defer resp.Body.Close()

				for {
					if processedCountMu.TryLock() {
						processedCount++
						processedCountMu.Unlock()
						break
					}
				}
			}(wg, i)

			time.Sleep(1 * time.Millisecond)
		}
		i++
	}
}

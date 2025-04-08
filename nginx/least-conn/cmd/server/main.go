package main

import (
	"context"
	"fmt"
	"log/slog"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"sync/atomic"
	"time"

	"github.com/labstack/echo/v4"
)

func main() {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt)
	defer stop()

	logger := slog.New(slog.NewTextHandler(os.Stdout, nil))
	slog.SetDefault(logger)

	// Setup
	e := echo.New()
	listener, err := net.Listen("tcp", ":8080")
	if err != nil {
		logger.Error("Failed to create listener", slog.String("error", err.Error()))
		return
	}
	e.Listener = &trackedListener{
		Listener:    listener,
		totalConns:  0,
		activeConns: 0,
	}
	e.GET("/metrics", func(c echo.Context) error {
		l := c.Echo().Listener.(*trackedListener)
		hostname, _ := os.Hostname()
		return c.String(
			http.StatusOK,
			fmt.Sprintf(
				"tcp_connections_active{host=\"%s\"} %d\ntcp_connections_total{host=\"%s\"} %d",
				hostname, l.ActiveConnections(), hostname, l.TotalConnections()))
	})
	e.GET("/", func(c echo.Context) error {
		l := c.Echo().Listener.(*trackedListener)
		hostname, _ := os.Hostname()

		if strings.Contains(hostname, "backend1") {
			logger.InfoContext(ctx, "Simulating a long-running process")
			time.Sleep(10 * time.Second)
			return c.JSON(http.StatusOK, fmt.Sprintf("OK(%d)", l.ActiveConnections()))
		}

		return c.JSON(http.StatusOK, fmt.Sprintf("OK(%d)", l.ActiveConnections()))
	})

	// Start server
	go func() {
		if err := e.Start(":8080"); err != nil && err != http.ErrServerClosed {
			panic(err)
		}
	}()

	// Wait for interrupt signal to gracefully shut down the server with a timeout of 10 seconds.
	<-ctx.Done()
	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Second)
	defer cancel()
	if err := e.Shutdown(ctx); err != nil {
		panic(err)
	}
}

type trackedListener struct {
	net.Listener
	totalConns  int64
	activeConns int64
}

func (l *trackedListener) Accept() (net.Conn, error) {
	conn, err := l.Listener.Accept()
	if err != nil {
		return nil, err
	}

	atomic.AddInt64(&l.totalConns, 1)
	atomic.AddInt64(&l.activeConns, 1)

	return &trackedConn{
		Conn: conn,
		onClose: func() {
			atomic.AddInt64(&l.activeConns, -1)
		},
	}, nil
}

func (l *trackedListener) ActiveConnections() int64 {
	return atomic.LoadInt64(&l.activeConns)
}
func (l *trackedListener) TotalConnections() int64 {
	return atomic.LoadInt64(&l.totalConns)
}

type trackedConn struct {
	net.Conn
	onClose func()
}

func (c *trackedConn) Close() error {
	err := c.Conn.Close()
	c.onClose()
	return err
}

package main

import "fmt"

type Server struct {
	Port int
	Host string
}

func (s *Server) Start() error {
	addr := fmt.Sprintf("%s:%d", s.Host, s.Port)
	fmt.Println("Starting on", addr)
	return nil
}

func (s *Server) Stop() {
	fmt.Println("Stopping server")
}

func helper() int {
	return 42
}

package main

import (
	"fmt"
	"net"
	"runtime"
	"time"
)

func main() {
	runtime.GOMAXPROCS(8)

	connection, err := net.Dial("tcp", "127.0.0.1:7479")
	if err != nil {
		fmt.Println("Error occured trying to establish connection")
		fmt.Println(err)
		return
	}
	fmt.Println("Connected to server")
	defer connection.Close()

	time.Sleep(time.Duration(5) * time.Second)
	msg := make([]byte, 100)
	copy(msg[:], "Hello Server!\n")
	connection.Write(msg)

	response := make([]byte, 1024)
	_, err = connection.Read(response)
	if err != nil {
		fmt.Println("Error occured trying to Read")
		fmt.Println(err)
		return
	}
	fmt.Println(string(response[:]))
}

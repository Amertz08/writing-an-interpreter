package object

import "testing"

func TestStringHashKey(t *testing.T) {
	hello1 := &String{Value: "hello"}
	hello2 := &String{Value: "hello"}

	diff1 := &String{Value: "world"}
	diff2 := &String{Value: "world"}

	if hello1.HashKey() != hello2.HashKey() {
		t.Errorf("expected hello1 and hello2 to have the same hash key")
	}

	if diff1.HashKey() != diff2.HashKey() {
		t.Errorf("expected diff1 and diff2 to have the same hash key")
	}
	if hello1.HashKey() == diff1.HashKey() {
		t.Errorf("expected hello1 and diff to have different hash keys")
	}
}

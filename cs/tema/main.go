package main

import (
	"crypto/sha256"
	"encoding/json"
	"fmt"
    "math/big"

	// See https://github.com/ing-bank/zkrp/blob/master/README.md
	"github.com/ing-bank/zkrp/bulletproofs"
)

// Un bloc dintr-un blockchain
type block struct {
	previous *block
	hash [32]byte
	data []byte
}

// Afișează blocul ca șir de caractere.
func (b block) String() string {
	return fmt.Sprintf("block{parent: %p, hash: %x, data: %v}", b.previous, b.hash, b.data)
}

// Transformă blocul într-un vector de bytes.
func (b *block) Bytes() []byte {
	return []byte(fmt.Sprintf("%v", *b))
}

// Calculează hash-ul SHA-256 al acestui bloc.
func (b *block) Hash() [32]byte {
	return sha256.Sum256(b.Bytes())
}

// Adaugă un nou bloc la lanț și îl returnează.
func (b *block) Append(data []byte) *block {
	return &block {
		previous: b,
		hash: b.Hash(),
		data: data,
	}
}

// Verifică dacă un bloc este valid.
func (b *block) Verify() bool {
	if (b.previous == nil) {
		return true
	}

	return b.previous.Verify() && b.previous.Hash() == b.hash
}

func main() {
	// Creez un lanț de blocuri
	first := block { nil, sha256.Sum256([]byte{}), []byte{} }
	second := first.Append([]byte{1, 2, 3})
	chain := second

	// Vrem să demonstrăm că output-ul tranzacției este un număr pozitiv.
	params, _ := bulletproofs.SetupGeneric(0, 4096)

	// Ne imaginăm că din tranzacție au rămas 50 de monede
	unspentTransactionOutput := new(big.Int).SetInt64(50)

	proof, _ := bulletproofs.ProveGeneric(unspentTransactionOutput, params)

	proofJson, _ := json.Marshal(proof)

	chain = chain.Append(proofJson)

	// Verificăm integritatea blockchain-ului
	fmt.Printf("Block chain valid: %t\n", chain.Verify())

	// Verificăm integritatea ultimei tranzacții
	var decodedProof bulletproofs.ProofBPRP
	_ = json.Unmarshal(chain.data, &decodedProof)
	ok, _ := decodedProof.Verify()
	fmt.Printf("Proof valid: %t\n", ok)
}

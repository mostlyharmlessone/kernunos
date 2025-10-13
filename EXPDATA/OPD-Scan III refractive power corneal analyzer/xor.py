def xor_compress(data, key):
    """Compresses data using XOR with a key."""
    compressed_data = bytearray()
    for byte in data:
        compressed_data.append(byte ^ key)
    return bytes(compressed_data)

def xor_decompress(compressed_data, key):
    """Decompresses data XORed with a key."""
    decompressed_data = bytearray()
    for byte in compressed_data:
        decompressed_data.append(byte ^ key)
    return bytes(decompressed_data)

# Example usage
#data = b"Hello, world!"
#key = 0x5A  # Example key
data = 0x4CA693
data = 3.150
key = 0x5A

compressed = xor_compress(data, key)
decompressed = xor_decompress(compressed, key)

print(f"Original data: {data}")
print(f"Compressed data: {compressed}")
print(f"Decompressed data: {decompressed}")

assert data == decompressed, "Compression/decompression failed"

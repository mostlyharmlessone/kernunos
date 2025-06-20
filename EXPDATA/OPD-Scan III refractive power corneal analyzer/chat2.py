# Convert hexadecimal string to decimal
hex_string = "AC23A5EAC239AE"
hex_string = "AC24B5EAC2564E"
decimal_value = int(hex_string, 16)
print(decimal_value)

def hex_to_float56(hex_string):
    # Convert the hexadecimal string to a 56-bit integer
    hex_value = int(hex_string, 16)

    # Extract the sign (1 bit)
    sign_bit = (hex_value >> 55) & 0x1

    # Extract the exponent (8 bits)
    exponent = (hex_value >> 47) & 0xFF

    # Extract the mantissa (47 bits)
    mantissa = hex_value & 0x7FFFFFFFFFFFFF  # mask the lower 47 bits

    # Calculate the bias for 8-bit exponent
    bias = 127

    # Reconstruct the floating-point value
    if exponent == 0:  # Denormalized number
        float_value = (-1)**sign_bit * (mantissa / (2**47)) * 2**(1 - bias)
    else:  # Normalized number
        float_value = (-1)**sign_bit * (1 + mantissa / (2**47)) * 2**(exponent - bias)

    return float_value

# Test with your hexadecimal sequence
hex_string = "AC23A5EAC239AE"
hex_string = "AC24B5EAC2564E"
floating_point_value = hex_to_float56(hex_string)
print(f"Floating-point value: {floating_point_value}")

def hex_to_float56(hex_string, bias=127, little_endian=False):
    # Convert the hexadecimal string to a 56-bit integer
    hex_value = int(hex_string, 16)

    # If little-endian, reverse the bytes
    if little_endian:
        # Convert to bytes and reverse the byte order
        hex_value = int.from_bytes(hex_value.to_bytes(7, byteorder='big'), byteorder='little')

    # Extract the sign (1 bit)
    sign_bit = (hex_value >> 55) & 0x1

    # Extract the exponent (8 bits)
    exponent = (hex_value >> 47) & 0xFF

    # Extract the mantissa (47 bits)
    mantissa = hex_value & 0x7FFFFFFFFFFFFF  # mask the lower 47 bits

    # Reconstruct the floating-point value
    if exponent == 0:  # Denormalized number
        float_value = (mantissa / (2**47)) * 2**(1 - bias)
    else:  # Normalized number
        float_value = (1 + mantissa / (2**47)) * 2**(exponent - bias)

    # Return the interpreted floating-point value
    return float_value

# Test with your hexadecimal sequence
hex_string = "AC23A5EAC239AE"
hex_string = "AC24B5EAC2564E"

# Test with Big-endian and Little-endian interpretations, and different bias values
for endian in [False, True]:  # False = Big-endian, True = Little-endian
    for bias_value in [127, 0]:  # Common bias (127) and no bias (0)
        floating_point_value = hex_to_float56(hex_string, bias=bias_value, little_endian=endian)
        endian_type = "Little-endian" if endian else "Big-endian"
        bias_type = "Bias 127" if bias_value == 127 else "Zero bias"
        print(f"Endianness: {endian_type}, Bias: {bias_type} -> Floating-point value: {floating_point_value}")

def hex_to_float56_alternative(hex_string, sign_bits=1, exp_bits=8, mantissa_bits=47, bias=127, little_endian=False):
    # Convert the hexadecimal string to a 56-bit integer
    hex_value = int(hex_string, 16)

    # If little-endian, reverse the bytes
    if little_endian:
        # Convert to bytes and reverse the byte order
        hex_value = int.from_bytes(hex_value.to_bytes(7, byteorder='big'), byteorder='little')

    # Extract the sign (1 bit)
    sign_bit = (hex_value >> (sign_bits + exp_bits + mantissa_bits - 1)) & 0x1

    # Extract the exponent (exp_bits)
    exponent = (hex_value >> mantissa_bits) & ((1 << exp_bits) - 1)

    # Extract the mantissa (mantissa_bits)
    mantissa = hex_value & ((1 << mantissa_bits) - 1)

    # Reconstruct the floating-point value
    if exponent == 0:  # Denormalized number
        float_value = (mantissa / (2**mantissa_bits)) * 2**(1 - bias)
    else:  # Normalized number
        float_value = (1 + mantissa / (2**mantissa_bits)) * 2**(exponent - bias)

    # Return the floating-point value
    return float_value

# Test with your hexadecimal sequence
#hex_string = "AC23A5EAC239AE"
#hex_string = "AC23A2EAC23A7E"
#hex_string = "AC23A5EAC239AE"
hex_string = "AC24B5EAC2564E"


# Test with different interpretations (different exponent bits, mantissa bits)
configurations = [
    {"sign_bits": 1, "exp_bits": 8, "mantissa_bits": 47, "bias": 127, "little_endian": False},  # Original assumption
    {"sign_bits": 1, "exp_bits": 2, "mantissa_bits": 54, "bias": 3, "little_endian": False},  # Custom small exponent
    {"sign_bits": 1, "exp_bits": 6, "mantissa_bits": 50, "bias": 63, "little_endian": False},  # Larger exponent range
    {"sign_bits": 1, "exp_bits": 5, "mantissa_bits": 50, "bias": 31, "little_endian": False},  # Another custom configuration
    {"sign_bits": 1, "exp_bits": 8, "mantissa_bits": 47, "bias": 127, "little_endian": True},  # Little-endian interpretation
]

# Run the different configurations
for config in configurations:
    floating_point_value = hex_to_float56_alternative(
        hex_string,
        sign_bits=config["sign_bits"],
        exp_bits=config["exp_bits"],
        mantissa_bits=config["mantissa_bits"],
        bias=config["bias"],
        little_endian=config["little_endian"]
    )
    
    print(f"Sign bits: {config['sign_bits']}, Exponent bits: {config['exp_bits']}, Mantissa bits: {config['mantissa_bits']}, Bias: {config['bias']}, Endian: {'Little' if config['little_endian'] else 'Big'} -> Floating-point value: {floating_point_value}")

# Helper function to interpret the number as a fixed-point
def interpret_fixed_point(hex_string, integer_bits=8, fractional_bits=48):
    # Convert hex string to an integer (56 bits)
    hex_value = int(hex_string, 16)

    # Extract the integer part (upper 28 bits)
    integer_part = hex_value >> fractional_bits

    # Extract the fractional part (lower 28 bits)
    fractional_part = hex_value & ((1 << fractional_bits) - 1)

    # Calculate the fixed-point value
    value = integer_part + (fractional_part / (2 ** fractional_bits))
    return value

# Helper function to interpret the number as BCD
def interpret_bcd(hex_string):
    # Convert the hex string into a 56-bit binary string
    binary_string = bin(int(hex_string, 16))[2:].zfill(56)

    # Group the binary string into 4-bit chunks
    bcd_digits = [binary_string[i:i+4] for i in range(0, 56, 4)]
    
    # Convert each 4-bit chunk to its decimal equivalent
    bcd_values = [str(int(digit, 2)) for digit in bcd_digits]

    # Join the BCD values into a string
    return ''.join(bcd_values)

# these are 56 bit groups from RA and ED
# Given hex sequence
hex_string = "AC24B5EAC2564E"
#hex_string = "AC23A2EAC23A7E"
#hex_string = "AC23A5EAC239AE"
#hex_string = "3C75B5E2CA9AAE"



# Interpret as Fixed-point with 28 bits for integer, 28 bits for fractional
fixed_point_value = interpret_fixed_point(hex_string, integer_bits=8, fractional_bits=48)

# Interpret as BCD
bcd_value = interpret_bcd(hex_string)

# Print the results
print(f"Fixed-point value (28 integer bits, 28 fractional bits): {fixed_point_value}")
print(f"BCD interpretation: {bcd_value}")


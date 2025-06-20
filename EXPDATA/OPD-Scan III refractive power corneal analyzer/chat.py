# Function for IEEE 754-like floating point (7-bit exponent, 20-bit mantissa)
def ieee754_float(bits):
    sign = int(bits[0], 2)
    exponent = int(bits[1:8], 2)  # 7 bits for exponent
    mantissa = int(bits[8:], 2)   # 20 bits for mantissa
    if exponent == 0:
        exp_val = -126
        mantissa_val = mantissa / (2 ** 20)
    else:
        exp_val = exponent - 127  # Bias of 127
        mantissa_val = 1 + mantissa / (2 ** 20)
    return (-1) ** sign * mantissa_val * (2 ** exp_val)


# this seems to work for the pupil center in PR
# Function for 5-bit exponent, 23-bit mantissa floating point
def small_exponent_float(bits):
    sign = int(bits[0], 2)
    exponent = int(bits[1:6], 2)  # 5 bits for exponent
    mantissa = int(bits[6:], 2)   # 23 bits for mantissa
    exp_val = exponent - 15  # Bias of 15 for 5-bit exponent
    mantissa_val = 1 + mantissa / (2 ** 23)
    return (-1) ** sign * mantissa_val * (2 ** exp_val)

# Function for Denormalized exponent with Bias 128
def denormalized_float(bits):
    sign = int(bits[0], 2)
    exponent = int(bits[1:8], 2)  # 7 bits for exponent
    mantissa = int(bits[8:], 2)   # 20 bits for mantissa
    if exponent == 0:
        exp_val = -128  # Bias of 128 for denormalized numbers
        mantissa_val = mantissa / (2 ** 20)
    else:
        exp_val = exponent - 128
        mantissa_val = 1 + mantissa / (2 ** 20)
    return (-1) ** sign * mantissa_val * (2 ** exp_val)

# Define the 28-bit binary strings for each number
first_num_bits = "1101001011000010011101000011"  # First number bitstring
second_num_bits = "1110001111000100100101010011"  # Second number bitstring

# Calculate floating-point values using different assumptions
first_num_ieee = ieee754_float(first_num_bits)
second_num_ieee = ieee754_float(second_num_bits)

first_num_small_exp = small_exponent_float(first_num_bits)
second_num_small_exp = small_exponent_float(second_num_bits)

first_num_denorm = denormalized_float(first_num_bits)
second_num_denorm = denormalized_float(second_num_bits)

# Print the results
print(f"IEEE 754-like (7-bit exponent, 20-bit mantissa):")
print(f"First number: {first_num_ieee}")
print(f"Second number: {second_num_ieee}")
print()
print(f"Small Exponent (5-bit exponent, 23-bit mantissa):")
print(f"First number: {first_num_small_exp}")
print(f"Second number: {second_num_small_exp}")
print()
print(f"Denormalized exponent with bias 128:")
print(f"First number: {first_num_denorm}")
print(f"Second number: {second_num_denorm}")
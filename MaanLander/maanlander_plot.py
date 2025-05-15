import re
import matplotlib.pyplot as plt

# Read the input text from file
with open("maanlander_output.txt", "r") as file:
    content = file.read()

# Extract parameters using regex
hoogte = list(map(float, re.findall(r"hoogte\s*=\s*([-\d.]+)", content)))
snelheid = list(map(float, re.findall(r"snelheid\s*=\s*([-\d.]+)", content)))
brandstof = list(map(float, re.findall(r"brandstof\s*=\s*([-\d.]+)", content)))

tijd = list(range(len(hoogte)))

# Create a 2x2 grid (one will be left blank)
fig, axs = plt.subplots(2, 2, figsize=(12, 10))
fig.suptitle("Simulatie van de Maanlander")

# Hoogte plot
axs[0, 0].plot(tijd, hoogte)
axs[0, 0].set_title("Hoogte (m)")

# Snelheid plot
axs[0, 1].plot(tijd, snelheid)
axs[0, 1].set_title("Snelheid (m/s)")

# Annotate last speed
last_t = tijd[-1]
last_snelheid = snelheid[-1]
axs[0, 1].annotate(f"{last_snelheid:.2f} m/s",
                   xy=(last_t, last_snelheid),
                   xytext=(last_t - 5, last_snelheid),
                   arrowprops=dict(arrowstyle="->", color='red'),
                   fontsize=10, color='red')

# Brandstof plot
axs[1, 0].plot(tijd, brandstof)
axs[1, 0].set_title("Brandstof (liter)")

# Hide the unused subplot
axs[1, 1].axis("off")

# Format axes
for ax in axs.flat:
    ax.set_xlabel("Tijdstap")
    ax.grid(True)

plt.tight_layout(rect=[0, 0.03, 1, 0.95])
plt.savefig("maanlander_simulatie.png")

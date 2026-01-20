import matplotlib.pyplot as plt

workers = [0, 1, 2, 4, 8]
means   = [0.7877, 0.8144, 0.4379, 0.2501, 0.2678]
stds    = [0.0104, 0.0109, 0.0089, 0.0046, 0.0258]

plt.figure()
plt.errorbar(workers, means, yerr=stds, fmt="-o")
plt.xlabel("num_workers")
plt.ylabel("time (seconds) for 100 batches")
plt.title("LFW DataLoader timing vs num_workers (heavy aug)")
plt.xticks(workers)

plt.tight_layout()
plt.savefig("timing_heavy.png", dpi=200)
print("Saved: timing_heavy.png")

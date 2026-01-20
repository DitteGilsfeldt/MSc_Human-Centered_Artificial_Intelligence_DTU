import matplotlib.pyplot as plt

workers = [0, 1, 2, 4, 8]
means   = [0.2791, 0.2994, 0.1639, 0.0956, 0.0960]
stds    = [0.0210, 0.0019, 0.0020, 0.0010, 0.0013]

plt.figure()
plt.errorbar(workers, means, yerr=stds, fmt="-o")
plt.xlabel("num_workers")
plt.ylabel("time (seconds) for 100 batches")
plt.title("LFW DataLoader timing vs num_workers (simple aug)")
plt.xticks(workers)

plt.tight_layout()
plt.savefig("timing_simple.png", dpi=200)
print("Saved: timing_simple.png")

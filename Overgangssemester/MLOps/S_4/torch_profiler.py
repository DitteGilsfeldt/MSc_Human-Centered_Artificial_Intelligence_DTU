import torch
import torchvision.models as models
from torch.profiler import profile, ProfilerActivity
from torch.profiler import profile, tensorboard_trace_handler, schedule

model = models.resnet18()
inputs = torch.randn(5, 3, 224, 224)

with profile(activities=[ProfilerActivity.CPU], record_shapes=True, schedule=schedule(wait=1, warmup=1, active=3, repeat=1), on_trace_ready=tensorboard_trace_handler("./log/resnet18")) as prof:
    for i in range(10):
        model(inputs)
        prof.step()

# prof.export_chrome_trace("trace.json")

# print(prof.key_averages().table(sort_by="cpu_time_total", row_limit=10))

print(prof.key_averages(group_by_input_shape=True).table(sort_by="self_cpu_memory_usage", row_limit=30))




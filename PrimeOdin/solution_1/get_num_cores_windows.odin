package main

import "core:sys/windows"

get_num_cores :: proc() -> int {
	sys_info := windows.SYSTEM_INFO{};
	windows.GetSystemInfo(&sys_info);
	return int(sys_info.dwNumberOfProcessors);
}

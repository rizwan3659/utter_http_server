
UTTER_C = utter.c
# CFLAGS = -O2 -g -Wall -I. -DSILENCE_PRINTF #We'll grade your code with these flags
# CFLAGS = -O0 -g -Wall -I. -fsanitize=thread #These flags are useful for debugging synchronizatino errors
# CFLAGS = -O0 -g -Wall -I. -fsanitize=address #These flags are useful for debugging memory errors
CFLAGS = -O0 -g -Wall # These flags are useful for debugging memory errors

utter: $(UTTER_C) dictionary.c dictionary.h csapp.c csapp.h more_string.c more_string.h
	$(CC) $(CFLAGS) -o utter $(UTTER_C) dictionary.c more_string.c csapp.c -pthread

clean:
	rm utter

#include <stdint.h>
#include <stdbool.h>
#include <malloc.h>
#include <string.h>
#include <stdio.h>

void skip_comment(FILE *in)
{
    /* todo */
}


/* 
   read whitespace or comment from in 
*/
bool skip_whitespace(FILE *in)
{
    bool read_ws;
    int c;

    c = fgetc(in);
    while( (c == 32) || (c >= 9 && c <= 13) )
    {
	read_ws = true;
	c = fgetc(in);
    }

    if(c == (int)'#')
    {
	skip_comment(in);
    }
    else
    {
	// push the last non-whitespace character read back onto the stream
	ungetc(c, in);
    }

    return read_ws;
}

uint8_t *read_ppm(char *path, int *w, int *h)
{
    uint8_t *rgb_data = NULL;
    char magic[2] = {0};
    int width, height, maxval;
    
    FILE *in = fopen(path, "r");

    if(in == NULL)
    {
	return NULL;
    }

    size_t readcnt = 0;
    readcnt = fread(magic, 1, 2, in);
    if(readcnt == 2 && magic[0] == 'P' && magic[1] == '6')
    {
	if(skip_whitespace(in))
	{
	    if((fscanf(in, "%d", &width) == 1) && skip_whitespace(in))
	    {
		if((fscanf(in, "%d", &height) == 1) && skip_whitespace(in))
		{
		    if((fscanf(in, "%d", &maxval) == 1) && skip_whitespace(in))
		    {
			int size = width * height * 3;

			printf("read %dx%d ppm file\n", width, height);
			
			*w = width;
			*h = height;
			rgb_data = (uint8_t *)malloc(size);
			if(rgb_data)
			{
			    readcnt = fread(rgb_data, 1, size, in);
			    if(readcnt != size)
			    {
				free(rgb_data);
				rgb_data = NULL;
			    }
			}
		    }
		}
	    }
	}
    }
    
    fclose(in);

    return rgb_data;
}

int write_ppm(char *path, int width, int height, uint8_t *data)
{
    FILE *out = fopen(path, "w");
    int err = 0;
    
    if(out)
    {
	int cnt;
	fprintf(out, "P6\n%d %d\n255\n", width, height);
	cnt = fwrite(data, 1, width * height * 3, out);
	if(cnt != (width * height * 3))
	{
	    err = -2;
	}
    }
    else
    {
	err = -1;
    }

    return err;
}
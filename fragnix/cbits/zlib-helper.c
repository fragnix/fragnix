#include <zlib.h>
#include <stdlib.h>

z_stream * streaming_commons_create_z_stream(void)
{
	z_stream *ret = malloc(sizeof(z_stream));
	if (ret) {
		ret->zalloc = Z_NULL;
		ret->zfree = Z_NULL;
		ret->opaque = Z_NULL;
		ret->next_in = NULL;
		ret->avail_in = 0;
		ret->next_out = NULL;
		ret->avail_out = 0;
	}
	return ret;
}

int streaming_commons_inflate_init2(z_stream *stream, int window_bits)
{
	return inflateInit2(stream, window_bits);
}

int streaming_commons_deflate_init2(z_stream *stream, int level, int methodBits,
                  int memlevel, int strategy)
{
	return deflateInit2(stream, level, Z_DEFLATED, methodBits, memlevel, strategy);
}

int streaming_commons_inflate_set_dictionary(z_stream *stream, const char* dictionary, 
                            unsigned int dictLength) {
        return inflateSetDictionary(stream, dictionary, dictLength);
}

int streaming_commons_deflate_set_dictionary(z_stream *stream, const char* dictionary, 
                            unsigned int dictLength) {
        return deflateSetDictionary(stream, dictionary, dictLength);
}

void streaming_commons_free_z_stream_inflate (z_stream *stream)
{
	inflateEnd(stream);
	free(stream);
}

void streaming_commons_set_avail_in (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_in = buff;
	stream->avail_in = avail;
}

void streaming_commons_set_avail_out (z_stream *stream, char *buff, unsigned int avail)
{
	stream->next_out = buff;
	stream->avail_out = avail;
}

int streaming_commons_call_inflate_noflush (z_stream *stream)
{
	return inflate(stream, Z_NO_FLUSH);
}

unsigned int streaming_commons_get_avail_in (z_stream *stream)
{
	return stream->avail_in;
}

unsigned int streaming_commons_get_avail_out (z_stream *stream)
{
	return stream->avail_out;
}

char* streaming_commons_get_next_in (z_stream *stream)
{
	return stream->next_in;
}

void streaming_commons_free_z_stream_deflate (z_stream *stream)
{
	deflateEnd(stream);
	free(stream);
}

int streaming_commons_call_deflate_noflush (z_stream *stream)
{
	return deflate(stream, Z_NO_FLUSH);
}

int streaming_commons_call_deflate_flush (z_stream *stream)
{
	return deflate(stream, Z_SYNC_FLUSH);
}

int streaming_commons_call_deflate_full_flush (z_stream *stream)
{
	return deflate(stream, Z_FULL_FLUSH);
}

int streaming_commons_call_deflate_finish (z_stream *stream)
{
	return deflate(stream, Z_FINISH);
}

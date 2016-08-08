
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

#define STATIC static

#define CAST(type,e) ((type)(e))

/* Java's >>> */
#define SIGNED_BITSHIFT_RIGHT_ZERO(stype, x, bits) (stype)(((unsigned stype)(x)) >> ((unsigned stype)(bits)))

/* Debugging */
#define DEBUG(msg,...) fprintf(stderr, msg "\n", ##__VA_ARGS__);
#define DEBUG_Along(msg, arg, len) debug_Along(msg, arg, len);
static void debug_Along (const char* msg, const long* v, int l) {
    fprintf(stderr, "%s: [", msg);
    int i;
    for (i=0; i<l; i++) {
        fprintf(stderr, "%ld", v[i]);
        if (i!= l-1) {
            fprintf(stderr, ", ");
        }
    }
    fprintf(stderr, "]\n");
}


typedef unsigned char byte;

static void skein_arraycopy_long(const long *in, int iIn, long *out, int iOut, int n) {
    assert(iIn >= 0);
    assert(iOut >= 0);
    assert(n >= 0);

    /* XXX check for number overflows here */
    memmove(&(out[iOut]), &(in[iIn]), n*sizeof(long));
}

static void skein_arraycopy_byte(const byte *in, int iIn, byte *out, int iOut, int n) {
    assert(iIn >= 0);
    assert(iOut >= 0);
    assert(n >= 0);

    /* XXX check for number overflows here */
    memmove(&(out[iOut]), &(in[iIn]), n*1);
}

static void skein_array_fill(byte *v, int len,
                             byte b) {
    assert(len >= 0);
    memset(v, b, len);
}

static void skein_array_fill_from_to_bytes(byte *v, int len,
					   int fromIndex, int toIndex, byte b) {
    assert(len >= 0);
    assert((0 <= fromIndex) && (fromIndex <= len));
    assert((0 <= toIndex) && (toIndex <= len));
    memset(&(v[fromIndex]), b, toIndex-fromIndex);
}



/* block function constants */

#define R00 ((int)(46))
#define R01 ((int)(36))
#define R02 ((int)(19))
#define R03 ((int)(37))
#define R10 ((int)(33))
#define R11 ((int)(27))
#define R12 ((int)(14))
#define R13 ((int)(42))
#define R20 ((int)(17))
#define R21 ((int)(49))
#define R22 ((int)(36))
#define R23 ((int)(39))
#define R30 ((int)(44))
#define R31 ((int)(9))
#define R32 ((int)(54))
#define R33 ((int)(56))
#define R40 ((int)(39))
#define R41 ((int)(30))
#define R42 ((int)(34))
#define R43 ((int)(24))
#define R50 ((int)(13))
#define R51 ((int)(50))
#define R52 ((int)(10))
#define R53 ((int)(17))
#define R60 ((int)(25))
#define R61 ((int)(29))
#define R62 ((int)(39))
#define R63 ((int)(43))
#define R70 ((int)(8))
#define R71 ((int)(35))
#define R72 ((int)(56))
#define R73 ((int)(22))
/* version 1, id-string 'SHA3' */
#define SCHEMA_VERSION ((long)(0x133414853L))
#define T1_FLAG_FINAL ((long)(1L << 63))
#define T1_FLAG_FIRST ((long)(1L << 62))
#define T1_FLAG_BIT_PAD ((long)(1L << 55))
#define T1_POS_TYPE ((long)(56))
#define TYPE_CONFIG ((long)(4L << T1_POS_TYPE))
#define TYPE_MESSAGE ((long)(48L << T1_POS_TYPE))
#define TYPE_OUT ((long)(63L << T1_POS_TYPE))
#define WORDS ((int)(8))
#define BYTES ((int)(8 * WORDS))
#define ROUNDS ((int)(72))
#define KS_PARITY ((long)(0x1BD11BDAA9FC1A22L))

static int MOD3[ROUNDS];
static int MOD9[ROUNDS];

static void skein_init_MOD_3_9 () {
    int i;
    for (i = 0; i < ROUNDS; i++) {
	MOD3[i] = i % 3;
	MOD9[i] = i % 9;
    }
}

struct Skein512 {
    /* size of hash result, in bits */
    int hashBitCount;
    /* current byte count in the buffer */
    int byteCount;
    /* tweak words: tweak0=byte count, tweak1=flags */
    long tweak0, tweak1;
    /* chaining variables */
    long x[WORDS];
    /* partial block buffer (8-byte aligned) */
    byte buffer[BYTES];
    /* key schedule: tweak */
    long tweakSchedule[5];
    /* key schedule: chaining variables */
    long keySchedule[17];
};

struct Skein512digest {
    byte bytes[BYTES];
    /* ^ XX expecting 64 bytes for 512 bits, is BYTES really the right source
       for this? Checking for it in skein_init */
};

STATIC
struct Skein512* skein_new_Skein512();

STATIC
void skein_hash_bytes(const byte *msg, int byteCount,
                      struct Skein512digest *digest);

STATIC
void skein_hash_chars(const char *msg, int byteCount,
                      struct Skein512digest *digest);

STATIC
void skein_hash_bits(const byte *msg, int bitCount,
                     struct Skein512digest *digest);

STATIC
void Skein512_update(struct Skein512 *this,
                     const byte *msg, int len);

STATIC
void Skein512_finalize(struct Skein512* this,
                       struct Skein512digest *hash);

static void Skein512_startNewType(struct Skein512* this,
                                  long type);

static void Skein512_processBlock(struct Skein512 *this,
                                  const byte *block, int off, int blocks, int bytes);

static long skein_rotlXor(long x, int n, long xor);

static void skein_setBytes(byte *dst, long *src, int byteCount);

static long skein_getLong(const byte *b, int len, int i);



/* ---------- Implementation ------------------------- */


static struct Skein512* INITIALIZED;

/* zero the allocations since the original Java code relied on this
   already */
#define LET_NEW(var,type) type* var= calloc(sizeof(type), 1)

static void skein_throw(const char *msg) {
    fprintf(stderr, "%s\n", msg);
    abort();
}

#define LET_XNEW(var,type) LET_NEW(var,type); \
                           if(!var) skein_throw("out of memory");


/* build/process the configuration block (only done once) */

static struct Skein512* _skein_new_Skein512 (int hashBitCount) {
    LET_XNEW(this, struct Skein512);

    this->hashBitCount = hashBitCount;
    Skein512_startNewType(this, TYPE_CONFIG | T1_FLAG_FINAL);

    /* set the schema, version */
    long w[2];
    w[0] = SCHEMA_VERSION;
    w[1] = hashBitCount;

    /* compute the initial chaining values from the configuration block */
    skein_setBytes(this->buffer, w, 2 * 8);
    Skein512_processBlock(this, this->buffer, 0, 1, 4 * WORDS);

    /* the chaining vars (x) are now initialized for the given hashBitLen.
       set up to process the data message portion of the hash (default)
       buffer starts out empty */
    Skein512_startNewType(this, TYPE_MESSAGE);

    return this;
}

static void skein_init_Skein512 () {
    INITIALIZED= _skein_new_Skein512(512);
}

STATIC
struct Skein512* skein_new_Skein512() {
    LET_XNEW(this,  struct Skein512);

    this->hashBitCount = INITIALIZED->hashBitCount;
    this->tweak0 = INITIALIZED->tweak0;
    this->tweak1 = INITIALIZED->tweak1;
    skein_arraycopy_long(INITIALIZED->x, 0, this->x, 0, WORDS);

    /* rely on LET_NEW to clear the remaining values to zero, like the
       Java version relies on the JVM to do this */

    return this;
}

/**
 * Calculate the hash code of the given message. Each bit in the message is processed.
 *
 * @param msg the message
 * @param digest the resulting hash code
 */
STATIC
void skein_hash_bytes(const byte *msg, int byteCount,
                      struct Skein512digest *digest) {
    assert(byteCount >= 0);
    assert(byteCount < (1 << (sizeof(byteCount)*8 - 3)));
    /* ^ XX is this correct or are we going to trigger positivity assert in skein_hash_bits ? */

    skein_hash_bits(msg, byteCount << 3, digest);
}

STATIC
void skein_hash_chars(const char *msg, int byteCount,
                      struct Skein512digest *digest) {
    return skein_hash_bytes(CAST(const byte *,msg), byteCount, digest);
}

/**
 * Calculate the hash code of the given message.
 *
 * @param msg the message
 * @param bitCount the number of bits to process
 * @param digest the resulting hash code
 */
STATIC
void skein_hash_bits(const byte *msg, int bitCount,
                     struct Skein512digest *digest) {
    assert(bitCount >= 0);

    /* >> should be fine here instead of >>>, no need for SIGNED_BITSHIFT_RIGHT_ZERO */
    int byteCount = bitCount >> 3;
    if ((bitCount & 7) != 0) {
        skein_throw("non-byte boundaries not implemented yet");
	/*
        int mask = 1 << (7 - (bitCount & 7));
	msg[byteCount] = (byte) ((msg[byteCount] & (-mask)) | mask);
	byteCount++;
            but msg is const currently, copy it or consume it?
        */
    }
    struct Skein512 *instance = skein_new_Skein512();
    Skein512_update(instance, msg, byteCount);
    if ((bitCount & 7) != 0) {
	instance->tweak1 |= T1_FLAG_BIT_PAD;
    }
    Skein512_finalize(instance, digest);
}

/* process the input bytes */
STATIC
void Skein512_update(struct Skein512 *this,
                     const byte *msg, int len) {
DEBUG("update: %d", len);
    const int origLen= len;

    int pos = 0;

    /* process full blocks, if any */
    if (len + this->byteCount > BYTES) {

	/* finish up any buffered message data */
	if (this->byteCount != 0) {
	    /* # bytes free in buffer */
	    int n = BYTES - this->byteCount;
	    if (n != 0) {
		skein_arraycopy_byte(msg, 0, this->buffer, this->byteCount, n);
                /* XX check for number overflows; currently relying on asserts below */
		len -= n;
		pos += n;
		this->byteCount += n;
	    }
	    Skein512_processBlock(this, this->buffer, 0, 1, BYTES);
	    this->byteCount = 0;
	}

	/* now process any remaining full blocks, 
	   directly from input message data */
	if (len > BYTES) {
	    /* number of full blocks to process */
	    int n = (len - 1) / BYTES;
	    Skein512_processBlock(this, msg, pos, n, BYTES);
	    len -= n * BYTES;
	    pos += n * BYTES;
	}
    }

    assert(pos >= 0);
    assert(len >= 0);

    /* copy any remaining source message data bytes into the buffer */
    if (len != 0) {
         assert((this->byteCount+len) < BYTES);
         assert(pos < origLen);
         assert((pos + len) <= origLen);
	skein_arraycopy_byte(msg, pos, this->buffer, this->byteCount, len);
	this->byteCount += len;
    }
DEBUG("/update: pos, byteCount, len: %d, %d, %d", pos, this->byteCount, len);
}

/* finalize the hash computation and output the result */
STATIC
void Skein512_finalize(struct Skein512* this,
                       struct Skein512digest *hash) {

    /* tag as the final block */
    this->tweak1 |= T1_FLAG_FINAL;

    /* zero pad if necessary */
    if (this->byteCount < BYTES) {
	skein_array_fill_from_to_bytes(this->buffer, BYTES,  this->byteCount, BYTES, (byte) 0);
    }

    /* process the final block */
DEBUG("process the final block");
    Skein512_processBlock(this, this->buffer, 0, 1, this->byteCount);

    /* now output the result
       zero out the buffer, so it can hold the counter */
    skein_array_fill(this->buffer, BYTES, 0);

    /* up to 512 bits are supported
       build the counter block */
    Skein512_startNewType(this, TYPE_OUT | T1_FLAG_FINAL);

    /* run 'counter mode' */
DEBUG("run 'counter mode'");
    Skein512_processBlock(this, this->buffer, 0, 1, 8);

    /* 'output' the counter mode bytes */
DEBUG("'output' the counter mode bytes");
    skein_setBytes(hash->bytes, this->x, (this->hashBitCount + 7) >> 3);
}

/* set up for starting with a new type */
static void Skein512_startNewType(struct Skein512* this,
                                  long type) {
    this->tweak0 = 0; this->tweak1 = T1_FLAG_FIRST | type;
}

static void Skein512_processBlock(struct Skein512 *this,
                                  const byte *block, int off, int blocks, int bytes) {
DEBUG("processBlock: %d, %d, %d", off, blocks, bytes);
    while (blocks-- > 0) {
	/* this implementation supports 2**64 input bytes (no carry out here)
	   update processed length */
	long *ts = this->tweakSchedule;
	this->tweak0 += bytes;
DEBUG("blocks, tweak0: %d, %ld", blocks, this->tweak0);
	int *mod3 = MOD3;
	int *mod9 = MOD9;
	ts[3] = ts[0] = this->tweak0; ts[4] = ts[1] = this->tweak1;
	ts[2] = this->tweak0 ^ this->tweak1;
	long *c = this->x;
	long *ks = this->keySchedule;
DEBUG_Along("ts", ts, 5);
DEBUG_Along("c", c, 8);
DEBUG_Along("ks", ks, 17);
	/* pre-compute the key schedule for this block */
	skein_arraycopy_long(c, 0, ks, 0, 8);
DEBUG_Along("ks after 1st arraycopy", ks, 17);
	skein_arraycopy_long(c, 0, ks, 9, 8);
DEBUG_Along("ks after 2nd arraycopy", ks, 17);
	ks[8] = KS_PARITY ^ c[7] ^ c[0] ^ c[1] ^ c[2] ^ c[3] ^ c[4] ^ c[5] ^ c[6];
DEBUG_Along("ks after KS_PARITY", ks, 17);
	/* do the first full key injection */
	long x0 = (c[0] = skein_getLong(block, BYTES, off)) + ks[0];
	long x1 = (c[1] = skein_getLong(block, BYTES, off + 8)) + ks[1];
	long x2 = (c[2] = skein_getLong(block, BYTES, off + 16)) + ks[2];
	long x3 = (c[3] = skein_getLong(block, BYTES, off + 24)) + ks[3];
	long x4 = (c[4] = skein_getLong(block, BYTES, off + 32)) + ks[4];
	long x5 = (c[5] = skein_getLong(block, BYTES, off + 40)) + ks[5] + this->tweak0;
	long x6 = (c[6] = skein_getLong(block, BYTES, off + 48)) + ks[6] + this->tweak1;
	long x7 = (c[7] = skein_getLong(block, BYTES, off + 56)) + ks[7];
DEBUG("x0, c[0]: %ld, %ld", x0, c[0]);
DEBUG("x1, c[1]: %ld, %ld", x1, c[1]);
DEBUG("x2, c[2]: %ld, %ld", x2, c[2]);
DEBUG("x3, c[3]: %ld, %ld", x3, c[3]);
DEBUG("x4, c[4]: %ld, %ld", x4, c[4]);
DEBUG("x5, c[5]: %ld, %ld", x5, c[5]);
DEBUG("x6, c[6]: %ld, %ld", x6, c[6]);
DEBUG("x7, c[7]: %ld, %ld", x7, c[7]);
	/* unroll 8 rounds */
        int r;
	for (r = 1; r <= ROUNDS / 4; r += 2) {
DEBUG("r, x1,x5: %d, %ld, %ld",r,x1,x5);
	    int rm9 = mod9[r], rm3 = mod3[r];
	    x1 = skein_rotlXor(x1, R00, x0 += x1); x3 = skein_rotlXor(x3, R01, x2 += x3);
	    x5 = skein_rotlXor(x5, R02, x4 += x5); x7 = skein_rotlXor(x7, R03, x6 += x7);
	    x1 = skein_rotlXor(x1, R10, x2 += x1); x7 = skein_rotlXor(x7, R11, x4 += x7);
	    x5 = skein_rotlXor(x5, R12, x6 += x5); x3 = skein_rotlXor(x3, R13, x0 += x3);
	    x1 = skein_rotlXor(x1, R20, x4 += x1); x3 = skein_rotlXor(x3, R21, x6 += x3);
	    x5 = skein_rotlXor(x5, R22, x0 += x5); x7 = skein_rotlXor(x7, R23, x2 += x7);
	    x1 = skein_rotlXor(x1, R30, x6 += x1) + ks[rm9 + 1];
	    x7 = skein_rotlXor(x7, R31, x0 += x7) + ks[rm9 + 7] + r;
	    x5 = skein_rotlXor(x5, R32, x2 += x5) + ks[rm9 + 5] + ts[rm3];
	    x3 = skein_rotlXor(x3, R33, x4 += x3) + ks[rm9 + 3];
	    x1 = skein_rotlXor(x1, R40, x0 += x1 + ks[rm9]);
	    x3 = skein_rotlXor(x3, R41, x2 += x3 + ks[rm9 + 2]);
	    x5 = skein_rotlXor(x5, R42, x4 += x5 + ks[rm9 + 4]);
	    x7 = skein_rotlXor(x7, R43, x6 += x7 + ks[rm9 + 6] + ts[rm3 + 1]);
	    x1 = skein_rotlXor(x1, R50, x2 += x1); x7 = skein_rotlXor(x7, R51, x4 += x7);
	    x5 = skein_rotlXor(x5, R52, x6 += x5); x3 = skein_rotlXor(x3, R53, x0 += x3);
	    x1 = skein_rotlXor(x1, R60, x4 += x1); x3 = skein_rotlXor(x3, R61, x6 += x3);
	    x5 = skein_rotlXor(x5, R62, x0 += x5); x7 = skein_rotlXor(x7, R63, x2 += x7);
	    x1 = skein_rotlXor(x1, R70, x6 += x1) + ks[rm9 + 2];
	    x7 = skein_rotlXor(x7, R71, x0 += x7) + ks[rm9 + 8] + r + 1;
	    x5 = skein_rotlXor(x5, R72, x2 += x5) + ks[rm9 + 6] + ts[rm3 + 1];
	    x3 = skein_rotlXor(x3, R73, x4 += x3) + ks[rm9 + 4];
	    x0 += ks[rm9 + 1]; x2 += ks[rm9 + 3];
	    x4 += ks[rm9 + 5]; x6 += ks[rm9 + 7] + ts[rm3 + 2];
	}
	/* do the final 'feed forward' xor, update context chaining vars */
	c[6] ^= x6;
	c[4] ^= x4;
	c[0] ^= x0;
	c[1] ^= x1;
	c[2] ^= x2;
	c[3] ^= x3;
	c[5] ^= x5;
	c[7] ^= x7;
	/* clear the start bit */
	this->tweak1 &= ~T1_FLAG_FIRST;
	off += BYTES;
    }
}

static long _skein_rotlXor(long x, int n, long xor) {
    return ((x << n) | SIGNED_BITSHIFT_RIGHT_ZERO(long, x, (64 - n))) ^ xor;
}

static long skein_rotlXor(long x, int n, long xor) {
    long res= _skein_rotlXor(x, n, xor);
DEBUG("rotlXor: %ld, %d, %ld, %ld", x, n, xor, res);
    return res;
}


static void skein_setBytes(byte *dst, long *src, int byteCount) {
    int n,i;
    for (n = 0, i = 0; n < byteCount; n += 8, i++) {
	long x = src[i];
	dst[n] = (byte) x;
	dst[n + 1] = (byte) (x >> 8);
	dst[n + 2] = (byte) (x >> 16);
	dst[n + 3] = (byte) (x >> 24);
	dst[n + 4] = (byte) (x >> 32);
	dst[n + 5] = (byte) (x >> 40);
	dst[n + 6] = (byte) (x >> 48);
	dst[n + 7] = (byte) (x >> 56);
    }
}

static long _skein_getLong(const byte *b, int len, int i) {
    if ((i < 0) || (i >= len + 8)) {
	skein_throw("ArrayIndexOutOfBounds");
    }
    /* XX optimize using casting? */
    return (((b[i] & 255) + ((b[i + 1] & 255) << 8) +
	((b[i + 2] & 255) << 16) + ((b[i + 3] & 255) << 24)) & 0xffffffffL) +
	(((b[i + 4] & 255) + ((b[i + 5] & 255) << 8) + ((b[i + 6] & 255) << 16) +
	((b[i + 7] & 255L) << 24)) << 32);
}

static long skein_getLong(const byte *b, int len, int i) {
    long res= _skein_getLong(b, len, i);
//DEBUG("getLong: len, i, res: %ld, %ld, %ld", len, i, res);
DEBUG("getLong: i, res: %d, %ld", i, res);
    return res;
}

static 
void skein_init () {
    assert(BYTES == 64);
    skein_init_MOD_3_9();
    skein_init_Skein512();
}



STATIC
void Skein512digest_print(struct Skein512digest *d,
			  FILE *out) {
    int i;
    for (i=0; i<BYTES; i++) {
	fprintf(out, "%x", d->bytes[i]);
    }
}

STATIC
void Skein512digest_println(struct Skein512digest *d,
			    FILE *out) {
    Skein512digest_print(d, out);
    fprintf(out, "\n");
}

int main (int argc, const char**argv) {
    skein_init();
    assert(argc==2);
    struct Skein512digest d;
    skein_hash_chars(argv[1], strlen(argv[1]), &d);
    Skein512digest_println(&d, stdout);
    return 0;
}

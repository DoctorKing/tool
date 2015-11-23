/*
 *
 *  Author: DQ
 *  Created: 11/23/15 3:20 PM
 *  Description:
 *
 */

import java.io.RandomAccessFile

object WavCutter extends App {
    val srcFile = args(0)
    val tgtFile = args(1)
    val start = args(2).toInt
    val end = args(3).toInt
    val numChannels: Short = 1
    val sampleRate = 16000
    val bitsPerSample = 16
    val srcRaf = new RandomAccessFile(srcFile, "rw")
    val tgtRaf = new RandomAccessFile(tgtFile, "rw")
    val bitsRate = sampleRate * bitsPerSample * numChannels
    val byteRate = sampleRate * bitsPerSample * numChannels / 8
    val audioSize = dateSize(srcRaf)
    val totalSeconds = math.round(audioSize.toDouble / byteRate).toInt
    if (start >= totalSeconds || start < 0) {
        throw new IllegalArgumentException(
            s"start seconds should be greater than 0 and less than total  $totalSeconds seconds")
    }
    val newAudioSeconds = math.min(totalSeconds, end) - start
    val newAudioSize = newAudioSeconds * byteRate
    writeHead(tgtRaf)
    writeDateSize(tgtRaf, newAudioSize)

    var dataStartPos = headSize + start * byteRate
    var dataEndPos = dataStartPos + newAudioSeconds * byteRate
    srcRaf.seek(dataStartPos)
    tgtRaf.seek(headSize)
    while (dataStartPos < dataEndPos) {
        tgtRaf.writeByte(srcRaf.readByte())
        dataStartPos += 1
    }
    srcRaf.close()
    tgtRaf.close()

    def headSize = 44

    def writeHead(raf: RandomAccessFile) = {
        raf.setLength(0)
        // ChunkID	4bytes	ASCII 码表示的“RIFF”。（0x52494646）
        raf.writeBytes("RIFF")
        // ChunkSize	4bytes	36+SubChunk2Size，或是 4 + ( 8 + SubChunk1Size ) + ( 8 + SubChunk2Size )，
        // 这是整个数据块的大小（不包括ChunkID和ChunkSize的大小）
        raf.writeInt(0)
        //Format	4bytes	ASCII 码表示的“WAVE”。（0x57415645）
        raf.writeBytes("WAVE")
        //SubChunk1ID		新的数据块（格式信息说明块）
        // ASCII 码表示的“fmt ”——最后是一个空格。（0x666d7420）
        raf.writeBytes("fmt ")
        //SubChunk1Size	4bytes	本块数据的大小（对于PCM，值为16）。
        raf.writeInt(Integer.reverseBytes(16))
        //AudioFormat	2bytes	PCM = 1 （比如，线性采样），如果是其它值的话，则可能是一些压缩形式
        raf.writeShort(java.lang.Short.reverseBytes(1.toShort))
        // NumChannels	2bytes	1 => 单声道  |  2 => 双声道
        raf.writeShort(java.lang.Short.reverseBytes(numChannels))
        // SampleRate	4bytes	采样率，如 8000，44100 等值
        raf.writeInt(Integer.reverseBytes(sampleRate))
        // ByteRate	4bytes	等于： SampleRate * numChannels * BitsPerSample / 8
        raf.writeInt(Integer.reverseBytes(byteRate))
        // BlockAlign	2bytes	等于：NumChannels * BitsPerSample / 8
        raf.writeShort(java.lang.Short.reverseBytes((numChannels * bitsPerSample / 8).toShort))
        // BitsPerSample	2bytes	采样分辨率，也就是每个样本用几位来表示，一般是 8bits 或是 16bits
        raf.writeShort(java.lang.Short.reverseBytes(bitsPerSample.toShort))
        //SubChunk2ID 4bytes	新数据块，真正的声音数 ASCII 码表示的“data ”——最后是一个空格。（0x64617461）
        raf.writeBytes("data")
        // SubChunk2Size	4bytes	数据大小，即，其后跟着的采样数据的大小。
        raf.writeInt(0)
    }

    def writeDateSize(raf: RandomAccessFile, audioLength: Int): Unit = {
        raf.seek(4)
        raf.writeInt(Integer.reverseBytes(36 + audioLength))
        raf.seek(40)
        raf.writeInt(Integer.reverseBytes(audioLength))
    }

    def dateSize(raf: RandomAccessFile) = {
        raf.seek(40)
        val ll = raf.readUnsignedByte()
        val lh = raf.readUnsignedByte()
        val hl = raf.readUnsignedByte()
        val hh = raf.readUnsignedByte()
        (hh << 24) + (hl << 16) + (lh << 8) + ll
    }
}


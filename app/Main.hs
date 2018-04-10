module Main where

{-# LANGUAGE ForeignFunctionInterface #-}

import qualified Sound.OpenAL.ALC.Capture as A
import Sound.OpenAL.AL.Buffer
import qualified Codec.Audio.Wave as W
import qualified Data.Set as S
import System.IO
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr)
import qualified Control.Concurrent as C
import Foreign.C.Types
import Data.Int
import Sound.OpenAL.ALC.BasicTypes
import qualified Control.Monad as M

waveSpec :: W.Wave
waveSpec = W.Wave { W.waveFileFormat = W.WaveVanilla
                  , W.waveSampleRate = 44100
                  , W.waveSampleFormat = W.SampleFormatPcmInt 16
                  , W.waveChannelMask = S.singleton W.SpeakerFrontCenter
                  , W.waveDataOffset = 0
                  , W.waveDataSize = 0
                  , W.waveSamplesTotal = 0
                  , W.waveOtherChunks = []
                  }

record :: Handle -> IO ()
record h = do
    possibleDevs <- A.allCaptureDeviceSpecifiers
    print possibleDevs

    device <- A.captureOpenDevice Nothing 44100 Mono16 4096
    recordAudio device

  where
    recordAudio (Just dev) = do
        devName <- A.captureDeviceSpecifier dev
        print devName

        buffer <- mallocArray 22050 :: IO (Ptr ALCbyte)
        A.captureStart dev

        --_ <- C.threadDelay 2500000

        M.forever $ do
            --_ <- C.threadDelay 50000
            samples <- A.captureNumSamples dev
            let (CInt length32) = samples
            let length = (fromIntegral length32 :: Int)

            --print length
            --print samples

            A.captureSamples dev buffer samples

            hPutBuf h buffer (2 * length)

        A.captureStop dev
        A.captureCloseDevice dev

        free buffer

        return ()
                                
    recordAudio Nothing = do
        print "No recording devices found"
        return ()


main :: IO ()
main = do 
    W.writeWaveFile "mic.wav" waveSpec record

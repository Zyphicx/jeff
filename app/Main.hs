module Main where

import qualified Control.Concurrent as C
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.Set as S
import Foreign.ForeignPtr as P

import qualified SDL
import qualified SDL.Audio as A

import qualified Codec.Audio.Wave as W

import qualified System.IO as IO

micSpec :: IO.Handle -> A.OpenDeviceSpec
micSpec h = A.OpenDeviceSpec { A.openDeviceFreq = A.Mandate 48000
                             , A.openDeviceFormat = A.Mandate A.Signed16BitNativeAudio
                             , A.openDeviceChannels = A.Mandate A.Mono
                             , A.openDeviceSamples = 4096
                             , A.openDeviceCallback = \_ (V.MVector size ptr) -> P.withForeignPtr ptr (\p -> IO.hPutBuf h p size)
                             , A.openDeviceUsage = A.ForCapture
                             , A.openDeviceName = Nothing
                             }


waveSpec :: W.Wave
waveSpec = W.Wave { W.waveFileFormat = W.WaveVanilla
                  , W.waveSampleRate = 48000
                  , W.waveSampleFormat = W.SampleFormatPcmInt 16
                  , W.waveChannelMask = S.singleton W.SpeakerFrontCenter
                  , W.waveDataOffset = 0
                  , W.waveDataSize = 0
                  , W.waveSamplesTotal = 0
                  , W.waveOtherChunks = []
                  }


record :: IO.Handle -> IO ()
record h = do
  SDL.initialize [SDL.InitAudio]
  (dev, _) <- A.openAudioDevice $ micSpec h
  A.setAudioDevicePlaybackState dev A.Play
  _ <- C.threadDelay 10000000
  return ()


main :: IO ()
main = do 
    W.writeWaveFile "mic.rec" waveSpec record

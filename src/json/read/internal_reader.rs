use std::{fmt, io::{Error, Read}, vec::IntoIter};

const ARRAY_DEFAULT: u8 = 0;

pub struct InternalReader<R> {
    read: R,
    buffer_size: usize,
    buffer_iter: (IntoIter<u8>, bool),
}

impl<R: Read> InternalReader<R> {
    pub fn new(mut read: R, buffer_size: usize) -> Result<Self, Error> {
        let buffer = InternalReader::read_data(&mut read, buffer_size)?;
        Ok(InternalReader {
            read,
            buffer_size,
            buffer_iter: buffer,
        })
    }

    fn read_data(read: &mut R, buffer_size: usize) -> Result<(IntoIter<u8>, bool), Error> {
        let mut buffer = vec![ARRAY_DEFAULT; buffer_size];
        let size = read.read(&mut buffer)?;
        buffer.truncate(size);
        Ok((buffer.into_iter(), size == buffer_size))
    }
}

impl<R: Read + fmt::Debug> fmt::Debug for InternalReader<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("JsonReader")
            .field("read", &self.read)
            .field("buffer_size", &self.buffer_size)
            .field("buffer", &self.buffer_iter)
            .finish()
    }
}

impl<R: Read> Iterator for InternalReader<R> {
    type Item = Result<u8, Error>;

    #[inline]
    fn next(&mut self) -> Option<Result<u8, Error>> {
        if self.buffer_size == 0 {
            return None;
        }
        loop {
            if let Some(item) = self.buffer_iter.0.next() {
                return Some(Ok(item));
            } else if self.buffer_iter.1 {
                match InternalReader::read_data(&mut self.read, self.buffer_size) {
                    Ok(item) => self.buffer_iter = item,
                    Err(err) => return Some(Err(err)),
                };
            } else {
                return None;
            }
        }
    }
}

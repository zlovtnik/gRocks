import gleam/file
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import gleam/string

// Utility functions for data processing

pub type PipelineError {
  FileError(String)
  ParseError(String)
  ValidationError(String)
}

pub fn attempt(result: Result(a, e), next: fn(a) -> Result(b, e)) -> Result(b, e) {
  case result {
    Ok(x) -> next(x)
    Error(y) -> Error(y)
  }
}

pub fn read_file(path: String) -> Result(String, PipelineError) {
  case file.read(path) {
    Ok(content) -> Ok(content)
    Error(_) -> Error(FileError("Failed to read file: " <> path))
  }
}

pub fn parse_data(content: String) -> Result(List(Float), PipelineError) {
  let lines = string.split(content, "\n")
  list.try_map(lines, fn(line) {
    case float.parse(line) {
      Ok(num) -> Ok(num)
      Error(_) -> Error(ParseError("Invalid number: " <> line))
    }
  })
}

pub fn validate_data(nums: List(Float)) -> Result(List(Float), PipelineError) {
  let valid = list.filter(nums, fn(num) { num >= 0.0 })
  case valid {
    [] -> Error(ValidationError("No valid numbers found"))
    _ -> Ok(valid)
  }
}

pub fn compute_average(nums: List(Float)) -> Result(Float, PipelineError) {
  case nums {
    [] -> Error(ValidationError("Cannot compute average of empty list"))
    _ -> {
      let sum = list.fold(nums, 0.0, fn(acc, num) { acc +. num })
      let count = list.length(nums)
      Ok(sum /. int.to_float(count))
    }
  }
}

pub fn error_to_string(err: PipelineError) -> String {
  case err {
    FileError(msg) -> "File Error: " <> msg
    ParseError(msg) -> "Parse Error: " <> msg
    ValidationError(msg) -> "Validation Error: " <> msg
  }
}

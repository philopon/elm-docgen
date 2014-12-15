var gulp      = require('gulp');
var less      = require('gulp-less');
var elm       = require('gulp-elm');
var uglifyjs  = require('gulp-uglifyjs');
var uglifycss = require('gulp-uglifycss');
var rename    = require('gulp-rename');
var plumber   = require('gulp-plumber');

// elm
gulp.task('compile-elm', function(){
  return gulp
    .src('elm/main.elm')
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest('dist/'));
});

gulp.task('minify-js', ['compile-elm'], function(){
  return gulp
    .src('dist/main.js')
    .pipe(uglifyjs())
    .pipe(rename({extname: '.min.js'}))
    .pipe(gulp.dest('dist/'));
});

gulp.task('watch-elm', function(){
  gulp.watch('elm/*.elm', ['compile-elm']);
});

// less
gulp.task('compile-less', function(){
  return gulp
    .src('style/main.less')
    .pipe(plumber())
    .pipe(less())
    .pipe(gulp.dest('dist/'));
});

gulp.task('minify-css', function(){
  return gulp
    .src('dist/main.css')
    .pipe(uglifycss())
    .pipe(rename({extname: '.min.css'}))
    .pipe(gulp.dest('dist/'));
});

gulp.task('watch-less', function(){
  gulp.watch('style/*.less', ['compile-less']);
});

gulp.task('default', ['minify-js', 'minify-css']);
gulp.task('devel', ['watch-elm', 'watch-less']);

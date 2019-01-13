#!/usr/bin/perl
#Ani Chaloyan, achaloya@ucsc.edu
use strict;
use warnings;
use Getopt::Std;
use POSIX qw(strftime);
$0 =~ s|.*/||;

#variables
my %macrosHash;
my %targetsHash;
my @splitMacro = ();
my @splitTarget = ();
my $found = 0;
my @prereqs = ();
my $firstTarget = "";
my $upToDate = 0;
my $percent = "";
my %options=();
my %lineNums;
my $num = 0;

#scan options 
getopts("dd:", \%options);

#print debug info if specified
if (defined $options{d})
{
    print "Debug info\n";
    print "File name: pmake\n";
    print "Targets: ";
    foreach my $t (@ARGV)
    {
        print $t . " ";
    }
    print "\n";
}

#open the Makefile
open(my $file, 'Makefile')
     or die "Could not open file 'Makefile'";

#put macros, targets in hash tables
while (my $line = <$file>)
{
    chomp $line;
    $num = $num + 1;
    $lineNums{$line} = $num;
    if (substr($line, 0, 1) eq '#') #comment
    {
        next;
    }
    elsif ($line =~ /=/) #macro
    {
        @splitMacro = split('=', $line);      
        $splitMacro[0] =~ s/\s+//g;               
        $splitMacro[1] =~ s/\s+//;

        if (index($line, '$') >= 0)
        {
            $splitMacro[1] = substitute($splitMacro[1], "");
        }

        $macrosHash{$splitMacro[0]} = $splitMacro[1];
    }
    elsif ($line =~ /:/  && index($line, "\t") < 0) #target
    {
        @splitTarget = split(/:/, $line);
        $splitTarget[0] =~ s/\s+//g;

        if (defined $splitTarget[1])
        {
            $splitTarget[1] =~ s/\s+//; 
            @prereqs = split / /, $splitTarget[1];
        }
        else 
        {
            @prereqs = ();
        }

        #possible macro substitution
        if (substr($splitTarget[0], 0, 1) eq '$')
        {
            $splitTarget[0] = $macrosHash{substr($splitTarget[0], 
                                                 2, -1)};
            $splitTarget[0] =~ s/\s+//; 
        }

        @{$targetsHash{$splitTarget[0]}} = @prereqs;

        #set first target
        if ($firstTarget eq "")
        {
            $firstTarget = $splitTarget[0];
        }
    }
}

#if no targets
if (!@ARGV)
{
    @ARGV = ($firstTarget);
}

foreach my $arg (@ARGV)
{
    #open the Makefile
    open(my $file, 'Makefile')
         or die "Could not open file 'Makefile'";

    while (my $line = <$file>)
    {
        chomp $line;
        my $ogLine = $line;
        if (substr($line, 0, 1) eq '#') #comment
        {
            next;
        }
        elsif ($line =~ /:/) #target
        {
            @splitTarget = split(/:/, $line);
            $splitTarget[0] =~ s/\s+//g;
            
            #possible macro substitution
            if (substr($splitTarget[0], 0, 1) eq '$')
            {
                $splitTarget[0] = $macrosHash{substr($splitTarget[0], 
                                                     2, -1)};
                $splitTarget[0] =~ s/\s+//; 
            }

            if ($splitTarget[0] eq $arg)
            {
                my @array = arraySub(@{$targetsHash{$arg}});
                foreach my $p (@array)
                {
                    if (exists $targetsHash{$p})
                    {    
                        jumpTo($p);
                    }
                    else 
                    {
                        if (defined mtime($p) && defined mtime($arg))
                        {
                            if (mtime($p) lt mtime($arg))
                            {
                                $upToDate = 1;
                            }
                            else 
                            {
                                $upToDate = 0;
                            }
                        }
                    }
                }
                if ($upToDate == 1)
                {
                    print "make: '$arg' is up to date.\n";
                    $upToDate = 0;
                }
                else
                {
                    $found = 1;
                }
            }
            else 
            {
                $found = 0;
            }
        }
        elsif ($line =~ /\t/ && $found == 1)
        {
            $line =~ s/^\s+//;
            if (index($line, '$') >= 0)
            {
                $line = substitute($line, $arg);
            }

            if (substr($line, 0, 1) eq '@')
            {
                system(substr($line, 2));
                if ($? != 0)
                {
                    die "make: *** [Makefile:$lineNums{$ogLine}: $arg] "
                    . "Error 1\n";
                }
            }
            elsif (substr($line, 0, 1) eq '-')
            {
                print substr($line, 2) . "\n";
                system(substr($line, 2));
                if ($? != 0)
                {
                    print "make: [Makefile:$lineNums{$ogLine}: $arg] "
                    . "Error 1 (ignored)\n";
                }
            }
            else
            {
                print $line . "\n";
                system($line);
                if ($? != 0)
                {
                    die "make: *** [Makefile:$lineNums{$ogLine}: $arg] "
                    . "Error 1\n";
                }
            }
        }
    }
}

sub substitute
{
    my ($macro, $file) = @_;
    my $temp = "";

    if (defined $macro)
    {
        my @words = split / /, $macro;
        foreach my $w (@words)
        {
            if (substr($w, 0, 1) eq '$')
            {
                if (substr($w, 1, 2) eq '<')
                {
                    my @x = @{$targetsHash{$file}};
                    my $firstP = $x[0];
                    if (substr($firstP, 0, 1) eq '%')
                    {
                        $firstP = $percent . substr($firstP, 
                                             index($firstP, '.'));
                    }
                    $temp = $temp . " " . $firstP;
                }
                elsif (substr($w, 1, 2) eq '@')
                {
                    $temp = $temp . " " . $file;
                }
                elsif (substr($w, 1, 2) eq '$')
                {
                    $temp = $temp . ' $';
                }
                elsif (defined $macrosHash{substr($w, 2, -1)})
                {
                    $temp = $temp . " " . $macrosHash{substr($w,
                                                      2, -1)};
                }
            }
            elsif (index($w, '\$$') >= 0)
            {
                 $temp = $temp . " " . substr($w, 0, 
                                       index($w, '$$')) . 
                               '$' . substr($w, (index($w, '\$$')+3));
            }
            else 
            {
                $temp = $temp . " " . $w;
            }
        }
    }

    $temp =~ s/\s+//; 
    return $temp;
}

sub jumpTo
{
    my ($prereq) = @_;

    #open the Makefile
    open(my $file, 'Makefile')
         or die "Could not open file 'Makefile'";

    while (my $line = <$file>)
    {
        chomp $line;
        my $ogLine = $line;
        if (substr($line, 0, 1) eq '#') #comment
        {
            next;
        }
        elsif ($line =~ /:/ && index($line, "\t") < 0) #target
        {
            @splitTarget = split(/:/, $line);
            $splitTarget[0] =~ s/\s+//g;
            
            #possible macro substitution
            if (substr($splitTarget[0], 0, 1) eq '$')
            {
                $splitTarget[0] = $macrosHash{substr($splitTarget[0], 
                                                     2, -1)};
            }

            if ($splitTarget[0] eq $prereq)
            {
                my @array = arraySub(@{$targetsHash{$prereq}});
                foreach my $p (@array)
                {
                    if (index($p, '.') >= 0 &&
                        exists $targetsHash{"%." . substr($p, 
                                           (index($p, '.')+1) )})
                    {
                        $percent = substr($p, 0, index($p, '.'));
                        jumpTo("%." . substr($p, (index($p, '.')+1) ));
                    }
                    elsif (exists $targetsHash{$p})
                    {    
                        jumpTo($p);
                    }
                    else 
                    {
                        if (substr($p, 0, 1) eq '%')
                        {
                            $p = $percent . substr($p, index($p, '.'));
                        }
                        if (substr($p, 0, 1) eq '%')
                        {
                            $prereq = $percent . substr($prereq, 
                                                 index($prereq, '.'));
                        }
                        if (defined mtime($p) && defined mtime($prereq))
                        {
                            if (mtime($p) lt mtime($prereq))
                            {
                                $upToDate = 1;
                            }
                            else 
                            {
                                $upToDate = 0;
                            }
                        }
                    }
                }
                if ($upToDate == 1)
                {
                    print "make: '$prereq' is up to date.\n";
                    $upToDate = 0;
                }
                else
                {
                    $found = 1;
                }
            }
            else 
            {
                $found = 0;
            }        
        }
        elsif ((index($line, "\t") >= 0) && $found == 1)
        {
            $line =~ s/^\s+//;
            if (index($line, '$') >= 0)
            {
                $line = substitute($line, $prereq);
            }

            if (substr($line, 0, 1) eq '@')
            {
                system(substr($line, 2));
                if ($? != 0)
                {
                    die "make: *** [Makefile:$lineNums{$ogLine}: "
                    . "$prereq] Error 1\n";
                }
            }
            elsif (substr($line, 0, 1) eq '-')
            {
                print substr($line, 2) . "\n";
                system(substr($line, 2));
                if ($? != 0)
                {
                    print "make: [Makefile:$lineNums{$ogLine}: "
                    . "$prereq] Error 1 (ignored)\n";
                }
            }
            else
            {
                print $line . "\n";
                system($line);
                if ($? != 0)
                {
                    die "make: *** [$prereq] Error 1\n";
                }
            }
        }
    }
}

sub arraySub 
{
    my (@array) = @_;
    my @temp = ();
    foreach my $a (@array)
    {
        #possible macro substitution
        if (substr($a, 0, 1) eq '$')
        {
            $a = $macrosHash{substr($a, 2, -1)};
        }
        my @words = split / /, $a;
        foreach my $w (@words)
        {
            push @temp, $w;
        }
    }

    return @temp;
}

sub mtime  
{
   my ($filename) = @_;
   my @stat = stat $filename;
   return @stat ? $stat[9] : undef;
}

sub fileinfo  
{
   my ($filename) = @_;
   my $mtime = mtime $filename;
   print "$filename: ";
   if (defined $mtime) {print strftime "%c\n", localtime $mtime}
                  else {print "$!\n"}
   return $mtime;
}



#!/usr/bin/env perl
# nanocode - minimal claude code alternative (Perl)
use strict; use warnings;
use JSON::PP; use LWP::UserAgent; use HTTP::Request;

my $KEY = $ENV{ANTHROPIC_API_KEY};
my $MODEL = $ENV{MODEL} || 'claude-sonnet-4-20250514';
my ($R,$B,$D,$C,$G,$BL) = ("\e[0m","\e[1m","\e[2m","\e[36m","\e[32m","\e[34m");

my %tools = (
    read => sub { my $a=shift; open my $f,'<',$a->{path} or return "error: $!"; my @l=<$f>; join '', map { ($_+1)."| $l[$_]" } 0..$#l },
    write => sub { my $a=shift; open my $f,'>',$a->{path} or return "error: $!"; print $f $a->{content}; 'ok' },
    edit => sub { my $a=shift; open my $f,'<',$a->{path} or return "error: $!"; my $t=join'',<$f>; close $f; 
        return 'error: not found' unless $t=~/$a->{old}/; $t=~s/\Q$a->{old}\E/$a->{new}/; 
        open $f,'>',$a->{path}; print $f $t; 'ok' },
    glob => sub { my $a=shift; join "\n", (glob("**/$a->{pat}"))[0..49] or 'none' },
    grep => sub { my $a=shift; my @h; for(glob('**/*')) { next if -d; open my $f,'<',$_ or next; my $i=1;
        while(<$f>) { push @h,"$_:$i:$_" if /$a->{pat}/; $i++ } } join "\n", @h[0..49] or 'none' },
    bash => sub { my $a=shift; `$a->{cmd} 2>&1` },
);

my @schema = (
    {name=>'read',description=>'Read file',input_schema=>{type=>'object',properties=>{path=>{type=>'string'}},required=>['path']}},
    {name=>'write',description=>'Write file',input_schema=>{type=>'object',properties=>{path=>{type=>'string'},content=>{type=>'string'}},required=>['path','content']}},
    {name=>'edit',description=>'Edit file',input_schema=>{type=>'object',properties=>{path=>{type=>'string'},old=>{type=>'string'},new=>{type=>'string'}},required=>['path','old','new']}},
    {name=>'glob',description=>'Find files',input_schema=>{type=>'object',properties=>{pat=>{type=>'string'}},required=>['pat']}},
    {name=>'grep',description=>'Search',input_schema=>{type=>'object',properties=>{pat=>{type=>'string'}},required=>['pat']}},
    {name=>'bash',description=>'Run command',input_schema=>{type=>'object',properties=>{cmd=>{type=>'string'}},required=>['cmd']}},
);

my $ua = LWP::UserAgent->new;

sub ask {
    my $msgs = shift;
    my $req = HTTP::Request->new(POST => 'https://api.anthropic.com/v1/messages');
    $req->header('Content-Type' => 'application/json', 'anthropic-version' => '2023-06-01', 'x-api-key' => $KEY);
    $req->content(encode_json({model=>$MODEL,max_tokens=>4096,system=>'Concise coding assistant',messages=>$msgs,tools=>\@schema}));
    decode_json($ua->request($req)->content);
}

print "${B}nanocode$R | $D$MODEL$R\n\n";
my @messages;

while (print "$B${BL}❯$R " and my $input = <STDIN>) {
    chomp $input;
    last if $input eq '/q';
    next unless $input;
    (@messages = (), print "${G}⏺ Cleared$R\n", next) if $input eq '/c';

    push @messages, {role=>'user',content=>$input};

    while (1) {
        my $resp = ask(\@messages);
        my @content = @{$resp->{content}||[]};
        my @results;

        for my $block (@content) {
            print "\n$C⏺$R $block->{text}" if $block->{type} eq 'text';
            if ($block->{type} eq 'tool_use') {
                my $name = $block->{name};
                print "\n$G⏺ $name$R\n";
                my $result = $tools{$name}->($block->{input});
                print "  $D⎿ ".(split /\n/,$result)[0]."$R\n";
                push @results, {type=>'tool_result',tool_use_id=>$block->{id},content=>$result};
            }
        }

        push @messages, {role=>'assistant',content=>\@content};
        last unless @results;
        push @messages, {role=>'user',content=>\@results};
    }
    print "\n";
}
